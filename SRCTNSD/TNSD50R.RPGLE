000100921019     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     F* TNSD50R *-----------------------------------------------------*
000300921019     F* RICHIESTA PARAMETRI PER STAMPA SEGNACOLLI/LETTERA DI VETTURA  *
000400921019     F*---------------------------------------------------------------*
000500941115     FTNSD50D   CF   E             WORKSTN
000600021029     FFIVABWKL  UF   E           K DISK
000700021029     F                                     RENAME(FIVAB000:FIVABWRK)
000800021029     FFIVAB46F  O  A E           K DISK
000900921026     FAZORG01L  IF   E           K DISK
001000921019     FTABEL00F  IF   E           K DISK
001100941228     FFNLV22P   O    F  132        PRINTER
001200131024     f
001300131024     D TES             S             15    DIM(3) CTDATA PERRCD(1)
001400131024     d
001500041130     d trul33ds      e ds
001600131023     d trul13ds      e ds
001700131023     D  tI13Tla      e                     EXTFLD(I13Tla)
001800131023     D  tO13Err      e                     EXTFLD(O13Err)
001900131023     D  tO13Msg      e                     EXTFLD(O13Msg)
002000131023     d
002100970714     D WLBDAT          DS                  INZ
002200970714     D  G02DAT                 1      8  0
002300970714     D  G02INV                 9     16  0
002400970714     D  G02ERR                17     17
002500970714     D  G02TGI                18     22  0
002600960104     D WGGMM           DS
002700960104     D  WGG                    1      2  0
002800960104     D  WMM                    3      4  0
002900131024     d
003000921019     D KPJBA         E DS
003100921019     D DSBLD         E DS
003200971223     D DSBLDE        E DS                  INZ
003300921019     D DS3A          E DS
003400941228     D DS5E          E DS
003500921019     D CNCR80        E DS
003600971231     D DSLV55        E DS                  EXTNAME(FNLV55DS)
003700921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800030626     D* DS ESTERA PER  STAMPA BOLLE
003900030626     d Fnlsb5ds      e ds
004000941228     D*
004100970714     D* DS PER LANCIO PGM CONTROLLO CAP
004200970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
004300970714     D DSLV13        E DS                  EXTNAME(FNLV13DS)
004400970714     D* DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO
004500970714     D DSLV14        E DS                  EXTNAME(FNLV14DS)
004600980123     D DSACO         E DS                  EXTNAME(CNACO00F)
004700980123     D DSIND         E DS                  EXTNAME(CNIND00F)
004800980123     D DSCLP         E DS                  EXTNAME(CNCLP00F)
004900980123     D DSCLS         E DS                  EXTNAME(FNCLS00F)
005000980123     D DSBS69        E DS                  EXTNAME(TIBS69DS)
005100131024     d
005200030626      * Parametri per richiesta stampanti
005300030626     d TRUL90DS      e ds
005400131022     d
005500131022     d datapiu         s              8  0
005600131022     d wdsp            s              8  0
005700921019     C****************************************************************
005800921019     C*  RIEPILOGO INDICATORI
005900921019     C***************************************************************
006000921102     C* 10    - ERRORE GENERICO SUL PRIMO FORMATO
006100921102     C* 20    - ERRORE GENERICO SECONDO FORMATO
006200921103     C* 21    - RICERCA NUMERO BOLLA
006300960104     C* 28    - CAMPO MESSAGGI
006400921102     C* 31/38 - ERRORE FORMATO  VIDEO 2
006500921102     C* 51/57 - CHAIN NON AVVENUTE
006600921102     C* 90    - INDICA LA RICHIESTA DI STAMPA VETTURA
006700921019     C*****************************************************************
006800921019     C     *ENTRY        PLIST
006900921019     C                   PARM                    KPJBA
007000030626     c                   Parm                    trul90ds
007100960118     C*
007200921019     C                   Z-ADD     1             CODUT
007300921019     C                   CALL      'X§PARUT'
007400921019     C                   PARM                    UT§DSE
007500921019     C                   MOVEL     RAGUT         RSUT             20
007600921019     C                   MOVEL     REC80         CNCR80
007700970714     C**
007800970714     C                   TIME                    W0140            14 0
007900970714     C                   MOVE      W0140         WDTGIO            8 0
008000970714     C*
008100970714     C                   Z-ADD     WDTGIO        G02DAT
008200970714     C                   MOVEL     *BLANK        G02ERR
008300970714     C                   CALL      'XSRDA8'
008400970714     C                   PARM                    WLBDAT
008500970714     C                   MOVEL     G02INV        DATEU             8 0
008600131022      * Richiamo pgm che calcola la prima data lavorativa disponibile
008700131022     c                   Clear                   Trul13ds
008800131023     c                   Eval      TI13Tla = 'L'
008900131022     c                   Eval      I13Mod = 'P'
009000131022     c                   Eval      I13Fil = 001
009100131022     c                   Eval      I13Din = dateu
009200171012     c                   Eval      I13gio = 2
009300131022     c                   Call      'TRUL13R'
009400131022     c                   Parm                    Trul13ds
009500131022     c
009600131023     c                   If        TO13Err = *Blanks and o13dfi>0
009700131022     c                   Eval      datapiu = O13Dfi
009800131022     c                   else
009900131022     c                   eval      datapiu=dateu
010000131022     c                   EndIf
010100921019     C*----------------------------------------------------*
010200131024     c* Imposta  testata
010300131024     C*----------------------------------------------------*
010400921019     C     KTAB2         KLIST
010500921019     C                   KFLD                    CODUT
010600921019     C                   KFLD                    COD               2
010700921019     C                   KFLD                    KEY               8
010800921019     C*----------------------------------------------------*
010900960104     C     *LIKE         DEFINE    VABRMN        NUMBOL
011000960104     C     *LIKE         DEFINE    VABRMN        COMRMN
011100921019     C*----------------------------------------------------*
011200921019     C* SE PROGRAMMA CHIAMATO VALORIZZO IL CAMPO DEL VIDEO
011300921019     C* CHE CONTIENE IL NUMERO BOLLA
011400921019     C*
011500960104     C                   MOVEL     KPJBU         NUMBOL
011600921103     C     NUMBOL        IFNE      *ZEROS
011700921103     C                   MOVEL     NUMBOL        VIDBOL
011800921103     C                   ELSE
011900921103     C                   MOVEL     *BLANKS       VIDBOL
012000921103     C                   END
012100131025     c
012200921102     C* VALORIZZO IL CAMPO DI RICHIESTA STAMPA LETTERA DI VETTURA
012300921102     C                   MOVEL     'S'           VSINOV
012400131022     C                   Z-ADD     WDTGIO        viddsp
012500131025     c
012600131025     c                   select
012700131025     c                   when      %subst(kpjbu:20:3)='ACQ'
012800131025     c                   eval      vidtes2=tes(1)
012900131025     c                   when      %subst(kpjbu:20:3)='SED'
013000131025     c                   eval      vidtes2=tes(2)
013100131025     c                   other
013200131025     c                   eval      vidtes2=tes(3)
013300131025     c                   endsl
013400131025     c
013500921102     C*
013600921019     C                   DO        *HIVAL
013700921026     C                   SETOFF                                       10
013800980518     C                   CLEAR                   UNO
013900921019     C* EMMETTO IL PRIMO FORMATO
014000941115     C                   EXFMT     TNSD501
014100921019     C* CMD3 FINE LAVORO
014200921026     C  NKC              EXSR      CRTFM1
014300921026     C*
014400960104     C* SE NON CI SONO ERRORI ED NON E' STATO DATO CMD 3
014500921026     C  N10
014600921026     CANNKC              DO
014700960104     C*
014800960105     C* RECUPERO I DATI DELLA BOLLA
014900960104     C                   EXSR      CARFM2
015000960104     C                   EXSR      CRTFM2
015100960104     C*
015200960104     C* SCRIVO AGGIORNO LE BOLLE CON I NUOVI DATI
015300960104     C* E LANCIO PGM SEGNACOLLI E STAMPA LETTERA DI VETTURA
015400960104     C*
015500960105     C  N10
015600960105     CANNKC              DO
015700921026     C                   EXSR      GESFLB
015800921026     C                   EXSR      GESSNC
015900921026     C   90              EXSR      GESLET
016000021029     C* DELETO   FIVABWKF   E SCRIVO FIVAB46F
016100021029     C**
016200021029     C                   WRITE     FIVAB000
016300131024     C**
016400921026     C                   Z-ADD     VNUMBO        COMRMN
016500921026     C*
016600021029     C     COMRMN        DELETE    FIVABWKL                           51
016700921026     C                   SETON                                        10
016800921026     C                   END
016900960105     C                   END
017000921026     C*
017100921026     C   10              END
017200921026     C*
017300960104     C*  CHIUSURA PGM E FINE
017400960104     C     *INKC         IFEQ      *ON
017500030626     C                   CLEAR                   Fnlsb5ds
017600960104     C                   MOVEL     'C'           DB0TLA
017700030626     c                   call      D90psl
017800030626     C                   PARM                    Fnlsb5ds
017900030626     C*
018000970714     C                   CLEAR                   DSLV13
018100970714     C                   MOVEL     'C'           I13TLA
018200970714     C                   CALL      'FNLV13R'
018300970714     C                   PARM                    KPJBA
018400970714     C                   PARM                    DSLV13
018500970714     C                   PARM                    DSSI95
018600971231     C*
018700971231     C                   MOVEL     'C'           D55TLA
018800971231     C                   CALL      'FNLV55R'
018900971231     C                   PARM                    DSLV55
019000980123     C*
019100980123     C                   MOVEL     'C'           I69TLA
019200980123     C                   CALL      'TIBS69R'
019300980123     C                   PARM                    DSBS69
019400041130     C*
019500041201     c                   clear                   trul33ds
019600041130     C                   MOVEL     'C'           I33TLA
019700041130     c                   movel(p)  trul33ds      kpjbu
019800041130     C                   CALL      'TRUL33R'
019900041130     C                   PARM                    KPJBA
020000960104     C*
020100960104     C                   SETON                                        LR
020200960104     C                   ENDIF
020300921026     C*---------------------------------------------------------------
020400921026     C*   CRTFM1  CONTROLLO FORMATO1
020500921026     C*---------------------------------------------------------------
020600921026     C     CRTFM1        BEGSR
020700921103     C*
020800960104     C* CONTROLLO SE RICHIESTA RICERCA NUMERO BOLLA
020900921103     C                   SETOFF                                           21
021000960104     C     '?'           SCAN      VIDBOL                                 21
021100921103     C   21              DO
021200921103     C                   SETON                                            10
021300941115     C                   CALL      'TNSD51R'
021400921103     C                   PARM                    KPJBA
021500921103     C                   MOVEL     KPJBU         VIDBOL
021600921103     C                   END
021700960104     C                   MOVEL     VIDBOL        VNUMBO
021800921026     C*
021900921026     C* CONTROLLLO SE RICHIESTA LA STAMPA DELLA LETTERA DI VETTURA
022000921103     C  N10VSINOV        IFEQ      'S'
022100921026     C                   SETON                                        90
022200131024     c                   else
022300131024     c                   setoff                                       90
022400921026     C                   END
022500131024     c
022600921026     C* CONTROLLO LA VERIDICITA' DEL NUMERO BOLLA
022700921103     C  N10              Z-ADD     VNUMBO        COMRMN
022800921026     C*
022900021029     C  N10COMRMN        CHAIN     FIVABWKL                           51
023000921103     C   51
023100921103     CANN10              SETON                                        3710
023200921026     C*
023300131022     c   10              leavesr
023400131024     c
023500131022     c* controllo data spedizione: max + 1 gg lavorativ
023600131022     C                   Z-ADD     viddsp        G02DAT
023700131022     C                   MOVEL     *BLANK        G02ERR
023800131022     C                   CALL      'XSRDA8'
023900131022     C                   PARM                    WLBDAT
024000131022    1C     G02ERR        IFEQ      '1'
024100131022     c                   seton                                        3910
024200131022     c                   leavesr
024300131022     c                   else
024400131022     C                   MOVEL     G02INV        wdsp              8 0
024500131022     c                   endif
024600131022
024700131022     c                   if        wdsp>datapiu
024800131024     c                             or wdsp<dateu
024900131022     c                   seton                                        3910
025000131022     c                   endif
025100131022     c
025200921026     C                   ENDSR
025300921026     C*---------------------------------------------------------------
025400921026     C*   CARFM2  CARICAMENTO FORMATO2
025500921026     C*---------------------------------------------------------------
025600921026     C     CARFM2        BEGSR
025700960916     C                   CLEAR                   SET
025800970714     C                   CLEAR                   AAA
025900970714     C                   CLEAR                   AAB
026000960916     C                   CLEAR                   VARCAP
026100970714     C                   CLEAR                   VARLOC
026200131024     c                   setoff                                       01
026300921026     C* COLLI
026400960104     C                   Z-ADD     VABNCL        VNUCOL
026500921026     C* PESO
026600960104     C                   Z-ADD     VABPKB        VPESO
026700921026     C* VOLUME
026800960104     C                   Z-ADD     VABVLB        VVOLUM
026900921026     C* NOTE
027000960104     C                   MOVEL     VABNOT        VNOTE
027100960104     C                   MOVEL     VABNT2        VNOTE2
027200921103     C* NATURA MERCE
027300960104     C                   MOVEL     VABNAS        VNATUR
027400921026     C* TIPO BOLLA
027500960918     C                   MOVEL     'A '          VTIBOL
027600960916     C* CAP       DESTINATARIO
027700960104     C                   MOVEL     VABCAD        VCAPDE
027800960916     C* NAZIONE   DESTINATARIO
027900960916     C                   MOVEL     VABNZD        VNAZDE
028000921026     C* LOCALITA' DESTINATARIO
028100960104     C                   MOVEL     VABLOD        VLOCDE
028200960105     C* LOCALITA' DESTINATARIO
028300960105     C                   MOVEL     VABIND        VINDIR
028400921026     C* PROVINCIA DESTINATARIO
028500960104     C                   MOVEL     VABPRD        VPRVDE
028600921026     C* RAGIONE SOCIALE DESTINATARIO
028700960104     C                   MOVEL     VABRSD        VRAGDE
028800131212     C                   MOVEL     VABRd2        VRAGDE2
028900921026     C* FERMO DEPOSITO
029000140411     c                   if        vabffd<>' '
029100921026     C                   MOVEL     'SI'          VFERMO
029200140411     c                   else
029300140411     c                   movel     'NO'          VFERMO
029400140411     c                   endif
029500131024     c
029600131024     c* se la data spedizione impostata è > data del giorno
029700131024     c                   if        wdsp>dateu
029800131024     c                   seton                                        01
029900131024     c                   endif
030000921026     C*
030100921026     C                   ENDSR
030200921026     C*---------------------------------------------------------------
030300921026     C*   CRTFM2  CONTROLLO FORMATO2
030400921026     C*---------------------------------------------------------------
030500921026     C     CRTFM2        BEGSR
030600921026     C*
030700921026     C* EMISSIONE DEL FORMATO 2
030800960916    0C                   DO        *HIVAL
030900921026     C*
031000941115     C                   EXFMT     TNSD502
031100960916     C                   SETOFF                                       202860
031200960916     C                   SETOFF                                       616465
031300960916     C                   SETOFF                                       6668
031400960916     C*
031500921026     C* PER CMD 12 ACCENDO IL 10
031600960916     C     *INKL         IFEQ      *ON
031700960916     C                   SETON                                        10
031800021029     C                   UNLOCK    FIVABWKL
031900960916     C                   ENDIF
032000921026     C*
032100960104     C  NKC
032200960104    1CANNKL              DO
032300921026     C*
032400921026     C* CONTROLLO  IL NUMERO COLLI
032500960104    2C     VNUCOL        IFEQ      *ZEROS
032600921026     C                   SETON                                          3120
032700960104    2C                   END
032800110506     C* non maggiore di 200
032900110506    2C     VNUCOL        IFgt      200
033000110506     C                   SETON                                          3520
033100110506    2C                   END
033200980518     C* SE MAGGIORE DI 35 ERRORE FORZABILE
033300980518    2C  N20VNUCOL        IFGT      35
033400980518     C     UNO           ANDEQ     ' '
033500980518     C                   SETON                                          3320
033600980518     C                   MOVEL     '1'           UNO               1
033700980518    2C                   END
033800921026     C* CONTROLLO  IL PESO
033900960104    2C  N20VPESO         IFEQ      *ZEROS
034000921026     C                   SETON                                          3220
034100960104    2C                   END
034200921026     C* CONTROLLO  IL TIPO BOLLA
034300960104    2C  N20VTIBOL        IFEQ      *BLANK
034400921026     C                   SETON                                          3420
034500960104   X2C                   ELSE
034600921026     C* CONTROLLO LA VERIDICITA' DEL CODICE
034700921026     C                   MOVEL     '3A'          COD
034800921026     C                   MOVEL     *BLANKS       KEY
034900921102     C                   MOVEL     VTIBOL        KEY
035000921026     C     KTAB2         CHAIN     TABEL                              52
035100921026     C* CONTROLLO SE ANNULLATO
035200960104    3C  N52TBLFLG        IFNE      ' '
035300921026     C                   SETON                                        52
035400960104    3C                   END
035500921026     C   52              SETON                                        3420
035600921026     C  N52              MOVEL     TBLUNI        DS3A
035700921026     C* VEDO SE CAUSALE UTILIZZABILE
035800960104    3C  N52§3AUCB        IFEQ      'N'
035900921026     C                   SETON                                        3420
036000960104    3C                   END
036100921026     C*
036200960104    2C                   END
036300960916     C* ERRORE --> FINE
036400960916     C   20              GOTO      ENDFM2
036500960916     C*** DESTINATARIO  ***
036600970714     C                   CLEAR                   DSLV14
036700970714     C                   MOVEL     VRAGDE        I14RSC
036800131212     C                   MOVEL     VRAGDE2       I14RS2
036900970714     C                   MOVEL     VINDIR        I14IND
037000970714     C                   MOVEL     VLOCDE        E14LOC
037100970714     C                   MOVEL     VPRVDE        E14PRV
037200970714     C                   MOVEL     VNAZDE        E14NAR
037300970714     C                   MOVEL     VCAPDE        E14CAP
037400970714     C                   CLEAR                   DSLV13
037500970714     C                   CLEAR                   DSSI95
037600970714     C*
037700970714     C                   CLEAR                   I14ISO
037800970714     C                   MOVEL     DATEU         I14DRI
037900970714     C                   CALL      'FNLV14R'
038000970716     C                   PARM                    KPJBA
038100970716     C                   PARM                    DSLV14
038200970714     C*
038300970714     C                   MOVEL     O14ERR        WERR              1
038400970714     C                   MOVEL     O14MSG        VIDMSG
038500970714     C                   EXSR      CTRERR
038600970714     C**
038700970714     C   20              GOTO      ENDFM2
038800970714     C**
038900941115     C* CONTROLLO CAP
039000960916     C     VCAPDE        IFNE      VARCAP
039100960916     C                   CLEAR                   SET
039200970714     C                   CLEAR                   AAA
039300970714     C                   CLEAR                   AAB
039400970714     C                   MOVEL     VCAPDE        VARCAP
039500970714     C     *LIKE         DEFINE    VCAPDE        VARCAP
039600960916     C                   ENDIF
039700970714     C     VLOCDE        IFNE      VARLOC
039800970714     C                   CLEAR                   AAA
039900970714     C                   CLEAR                   AAB
040000970714     C                   MOVEL     VLOCDE        VARLOC
040100970714     C     *LIKE         DEFINE    VLOCDE        VARLOC
040200970714     C                   ENDIF
040300960916     C**
040400960104     C                   EXSR      CHKCAP
040500970714     C**
040600970714     C   20              GOTO      ENDFM2
040700921026     C*
040800980102     C     VABLNA        IFEQ      *ZEROS
040900970714     C                   MOVEL     O95LNA        VABLNA
041000970714     C                   MOVEL     O95ZNC        VABZNC
041100980519     C                   ELSE
041200980519     C**
041300980519     C     VABZNC        IFEQ      00
041400980519     C     VABLNA        ANDEQ     O95LNA
041500980519     C                   MOVEL     O95ZNC        VABZNC
041600980519     C                   ENDIF
041700980102     C                   END
041800980519     C**
041900970714     C                   MOVEL     O95CTS        VABCTS
042000110622     C***                MOVEL     O95ISO        WFIN              1
042100960104    1C                   ENDDO
042200921026     C*
042300960916     C     ENDFM2        TAG
042400960916    0C   20              ENDDO
042500921026     C*
042600921026     C                   ENDSR
042700970714     C*------------------------------------------------------------------------*
042800970714     C* ERRORE O RICERCA CAMPO
042900970714     C*------------------------------------------------------------------------*
043000970714     C     CTRERR        BEGSR
043100970714     C* IMPOSTO CAMPI PASSATI
043200970714     C     O13RON        IFEQ      'S'
043300970714     C                   MOVEL     O95NAR        VNAZDE
043400970714     C                   ENDIF
043500970714     C     O13ROC        IFEQ      'S'
043600970714     C                   MOVEL     O95CAP        VCAPDE
043700970714     C                   ENDIF
043800970714     C     O13ROP        IFEQ      'S'
043900970714     C                   MOVEL     O95PRV        VPRVDE
044000970714     C                   ENDIF
044100970714     C     O13ROL        IFEQ      'S'
044200970714     C                   MOVEL     O95LOC        VLOCDE
044300970714     C                   ENDIF
044400970714     C*
044500970714    1C     WERR          IFNE      ' '
044600970714     C*
044700970714    2C     VIDMSG        IFNE      *BLANKS
044800970714     C                   SETON                                        28
044900970714    2C                   ENDIF
045000970714     C* POSIZIONAMENTO CURSORE
045100970714    2C     WERR          IFEQ      '1'                                          R.SOCIALE
045200970714     C                   SETON                                        64
045300970714    2C                   ENDIF
045400970714     C     WERR          IFEQ      '2'                                          INDIRIZZO
045500970714     C                   SETON                                        61
045600970714    2C                   ENDIF
045700970714    2C     WERR          IFEQ      '3'                                          LOCALITA
045800970714     C                   SETON                                        66
045900970714    2C                   ENDIF
046000970714    2C     WERR          IFEQ      '4'                                          PROVINCIA
046100970714     C                   SETON                                        60
046200970714    2C                   ENDIF
046300970714    2C     WERR          IFEQ      '5'                                          CAP
046400970714     C                   SETON                                        65
046500970714    2C                   ENDIF
046600970714    2C     WERR          IFEQ      '6'                                          NAZIONE
046700970714     C                   SETON                                        68
046800970714    2C                   ENDIF
046900970714     C*
047000970714     C                   SETON                                        20
047100970714    1C                   ENDIF
047200970714    1C                   ENDSR
047300941115     C*------------------------------------------------------------------------*
047400941115     C*  CHKCAP - CONTROLLO FORMALE DEL CAP
047500941115     C*------------------------------------------------------------------------*
047600941115     C     CHKCAP        BEGSR
047700941115     C*
047800970714     C                   MOVEL     'S'           I13AF0
047900970714     C                   MOVEL     'S'           I13AF1
048000970714     C                   MOVEL     'S'           I13SZ2
048100970714     C                   MOVEL     'S'           I13CNV
048200970714     C                   MOVEL     'S'           I13SZV
048300970714     C                   MOVEL     SET           E13FZV
048400970714     C                   MOVEL     AAA           E13FZ1
048500970714     C                   MOVEL     AAB           E13FZ2
048600970714     C                   MOVEL     '7'           I95TCN                                     LLO FORM
048700970714     C                   MOVEL     VCAPDE        I95CAP                         *CAP
048800970714     C                   MOVEL(P)  VLOCDE        I95LOC
048900970714     C                   Z-ADD     VPESO         I95LKG
049000970714     C                   Z-ADD     VVOLUM        I95LMC
049100970714     C                   MOVEL     VPRVDE        I95PRV
049200970714     C                   MOVEL     VNAZDE        I95NAR
049300970714     C                   MOVEL     DATEU         I95DAT
049400971201     C     §3AFCA        IFNE      0
049500971201     C                   MOVEL     'S'           I95FCA
049600971201     C                   ENDIF
049700971201     C                   MOVEL     §3ATB1        I95TPO
049800980521     C* SERVIZIO CORRIERE
049900980521     C                   MOVEL     'C'           I95TSP
050000131024     c
050100980521     C     VABLNA        IFGT      0
050200980521     C     VABLNA        CHAIN     AZORG01L                           54
050300980521     C                   ELSE
050400981229     C* SE NON C'E' LA LNA DO PER SCONTATO CHE SIA BARTOLINI
050500980521     C                   MOVEL     'BAR'         ORGDIT
050600980521     C                   ENDIF
050700980521     C* FERMO DEPOSITO
050800960104     C     VFERMO        IFEQ      'SI'
050900970714     C                   MOVEL     'S'           I95FFD
051000980521     C                   ELSE
051100980521     C     ORGDIT        IFEQ      'BAR'
051200980521     C                   MOVEL     'E'           I95TSP
051300960104     C                   ENDIF
051400980521     C                   ENDIF
051500131024     c
051600020225     C                   Z-ADD     102           WLNP              3 0
051700980602     C* TERMINAL DI PARTENZA
051800980602     C                   CLEAR                   DSLV55
051900980602     C                   MOVEL     'P'           D55TPT
052000980602     C                   MOVE      WLNP          D55LIN
052100980602     C                   Z-ADD     DATEU         D55DRF
052200980602     C                   CALL      'FNLV55R'
052300980602     C                   PARM                    DSLV55
052400000623     C*
052500980602     C                   MOVE      D55TFP        I95TFP
052600000623     C**
052700980602     C                   MOVEL     'S'           I95FRE
052800960104     C*
052900970714     C                   CALL      'FNLV13R'
053000970714     C                   PARM                    KPJBA
053100970714     C                   PARM                    DSLV13
053200970714     C                   PARM                    DSSI95
053300970714     C*
053400970714     C                   MOVEL     O13ERR        WERR              1
053500970714     C                   MOVEL     O13MSG        VIDMSG
053600970714     C                   EXSR      CTRERR
053700970714     C**
053800970714     C                   MOVEL     E13FZV        SET               1            DESTINATARIO
053900970714     C                   MOVEL     E13FZ1        AAA               1
054000970714     C                   MOVEL     E13FZ2        AAB               1
054100941115     C*
054200941115     C                   ENDSR
054300921026     C*---------------------------------------------------------------
054400021029     C*   GESFLB  GESTIONE ARCHIVIO FIVABWKF
054500921026     C*---------------------------------------------------------------
054600921026     C     GESFLB        BEGSR
054700921026     C*
054800021029     C* VALORIZZO I CAMPI NON ANCORA VALORIZZATI DI FIVABWKF
054900921026     C*
055000921026     C* ANNO SPEDIZIONE
055100131022     C***                TIME                    W0140            14 0
055200960104     C* UDATE IN GGMMAAAA
055300131022     C***                MOVE      W0140         WDTGIO            8 0
055400131022     C***                MOVE      WDTGIO        VABAAS
055500131022     C***                MOVEL     WDTGIO        WGGMM
055600921026     C* MESE/GIORNO SPEDIZIONE
055700131022     C***                MOVEL     WMM           VABMGS
055800131022     C***                MOVE      WGG           VABMGS
055900131022     c
056000131022     c                   movel     wdsp          vabaas
056100131022     c                   move      wdsp          vabmgs
056200131022     c
056300960104     C                   Z-ADD     102           VABLNP
056400021029      *
056500021029      * P.O di gestione
056600021029     C                   Z-ADD     102           VABFGS
056700921026     C* NUMERO SERIE SPEDIZIONE
056800960104     C                   Z-ADD     01            VABNRS
056900921026     C* NUMERO SPEDIZIONE
057000041201     c* lo prendo da AZNUM con trul33r
057100041130     C                   CLEAR                   TRUL33DS
057200041130     C                   Z-ADD     103           I33CNU
057300041130     C                   Z-ADD     vablnp        I33PO1
057400041130     C                   Z-ADD     1             I33NUM
057500041130     c                   movel(p)  trul33ds      kpjbu
057600041130     c                   call      'TRUL33R'
057700041130     c                   parm                    kpjba
057800041130     c                   movel     kpjbu         trul33ds
057900041130     C                   IF        O33ERR = 0
058000041130     C                   Z-ADD     O33Nrf        vabnsp
058100041130     C                   ELSE
058200041130     C                   CLEAR                   VABNSP
058300041130     C                   ENDIF
058400041130     C
058500921026     C* CODICE BOLLA PARTENZA
058600960104     C                   MOVE      VTIBOL        VABCBO
058700131212     c                   movel(p)  vragde        vabrsd
058800131212     c                   movel(p)  vragde2       vabrd2
058900921026     C*     CAP DESTINATARIO
059000960104     C                   MOVEL     VCAPDE        VABCAD
059100960916     C* NAZIONE DESTINATARIO
059200960916     C                   MOVEL     VNAZDE        VABNZD
059300921026     C* LOCALITA' DESTINATARIO
059400960104     C                   MOVEL     VLOCDE        VABLOD
059500960105     C* LOCALITA' DESTINATARIO
059600960105     C                   MOVEL     VINDIR        VABIND
059700921026     C* PROVINCIA DESTINATARIO
059800960104     C                   MOVEL     VPRVDE        VABPRD
059900921026     C* TIPO SPEDIZIONE
060000980521     C                   MOVEL     I95TSP        VABTSP
060100921026     C* NUMERO COLLI
060200960104     C                   Z-ADD     VNUCOL        VABNCL
060300921026     C* PESO
060400960104     C                   Z-ADD     VPESO         VABPKB
060500921026     C* VOLUME
060600960104     C                   Z-ADD     VVOLUM        VABVLB
060700960308     C* COD.MITTENTE  (SE GIA' NON IMPOSTATO)
060800960308     C     VABCCM        IFEQ      *ZERO
060900980121     C                   Z-ADD     0000001       VABCCM
061000980121     C                   MOVEL     VABLNP        VABCCM
061100960308     C                   END
061200921026     C* RICERCO IL NUMERO SEGNACOLLI E LANCIO IL PROGRAMMA STAMPA
061300921026     C* SEGNACOLLI
061400041201     c* lo prendo da AZNUM con trul33r
061500041201     C                   CLEAR                   TRUL33DS
061600041201     C                   Z-ADD     109           I33CNU
061700041201     C                   Z-ADD     vablnp        I33PO1
061800041201     C                   Z-ADD     vabncl        I33NUM
061900041201     c                   movel(p)  trul33ds      kpjbu
062000041201     c                   call      'TRUL33R'
062100041201     c                   parm                    kpjba
062200041201     c                   movel     kpjbu         trul33ds
062300041201     C                   IF        O33ERR<> 0
062400921026     C*
062500960104     C                   Z-ADD     0             VABNCD
062600960104     C                   Z-ADD     0             VABNCA
062700960104     C                   ELSE
062800041201     c*
062900041201     c                   z-add     o33nri        vabncd
063000041201     c                   z-add     o33nrf        vabnca
063100921026     C                   END
063200921026     C*
063300921026     C* SE AL = DAL AZZERO IL CAMPO
063400960104     C     VABNCA        IFEQ      VABNCD
063500960104     C                   Z-ADD     0             VABNCA
063600921026     C                   END
063700921026     C* NOTE
063800960104     C                   MOVEL     VNOTE         VABNOT
063900960104     C                   MOVEL     VNOTE2        VABNT2
064000921103     C* NATURA MERCE
064100960104     C                   MOVEL     VNATUR        VABNAS
064200921026     C* CODICE TRATTAMENTO MERCE
064300960104     C   90              MOVEL     '3 '          VABCTM
064400921026     C*
064500960104     C  N90              MOVEL     '2 '          VABCTM
064600921026     C* CODICE FERMO DEPOSITO
064700921026     C     VFERMO        IFEQ      'SI'
064800960104     C                   MOVEL     'S'           VABFFD
064900921026     C                   ELSE
065000960104     C                   MOVE      ' '           VABFFD
065100921026     C                   END
065200021029     C*  AGGIORNO IL FILE FIVABWKF
065300021029     C                   UPDATE    FIVABWRK
065400921026     C*
065500921026     C                   ENDSR
065600921026     C*---------------------------------------------------------------
065700921026     C*   GESSNC  GESTIONE STAMPA SEGNACOLLI
065800921026     C*---------------------------------------------------------------
065900921026     C*
066000921026     C     GESSNC        BEGSR
066100921026     C*
066200960104     C                   Z-ADD     VABNCD        CO2SCD            7 0
066300960104     C     VABNCA        IFEQ      0
066400960104     C                   Z-ADD     VABNCD        VABNCA
066500921104     C                   END
066600921104     C*
066700921026     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
066800921026     C*
066900921026     C                   Z-ADD     0             §LDNC1
067000960104     C                   MOVEL     VABLNP        §LDLNP
067100960104     C     VABLNP        CHAIN     AZORG                              56
067200980128    1C     *IN56         IFEQ      *OFF
067300980128    2C     ORGDE5        IFNE      *BLANKS
067400980128     C                   MOVEL     ORGDE5        §LDDLP
067500980128   X2C                   ELSE
067600980128     C                   MOVEL     ORGDES        §LDDLP
067700980128    2C                   END
067800980128   X1C                   ELSE
067900980128     C                   MOVEL     *BLANKS       §LDDLP
068000980128    1C                   END
068100921026     C                   MOVEL     *BLANK        §LDCDP
068200960104     C                   MOVEL     VABNRS        §LDNRS
068300960104     C                   MOVEL     VABLNA        §LDLNA
068400960104     C     VABLNA        CHAIN     AZORG                              56
068500921026     C   56              MOVEL     *BLANKS       §LDDLA
068600970129    1C  N56ORGDE5        IFNE      *BLANKS
068700970129     C                   MOVEL     ORGDE5        §LDDLA
068800970129     C                   ELSE
068900970129     C                   MOVEL     ORGDES        §LDDLA
069000970129    1C                   END
069100971231     C                   CLEAR                   DSLV55
069200971231     C                   MOVEL     'A'           D55TPT
069300971231     C                   MOVE      VABLNA        D55LIN
069400971231     C                   MOVE      VABMGS        D55DRF
069500971231     C                   MOVEL     VABAAS        D55DRF
069600971231     C                   CALL      'FNLV55R'
069700971231     C                   PARM                    DSLV55
069800971231     C                   MOVEL     D55TFA        §LDTNA
069900960104     C                   MOVEL     VABTSP        §LDTSP
070000960104     C                   MOVEL     VABZNC        §LDZSC
070100960104     C                   MOVE      VABNCL        §LDNC2
070200980123     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
070300980123     C                   CLEAR                   DSBS69
070400980123     C                   Z-ADD     VABCCM        I69KAC
070500980123     C                   Z-ADD     VABCCM        I69KIN
070600980123     C                   MOVEL     KNSIF         I69SIF
070700980123     C                   CALL      'TIBS69R'
070800980123     C                   PARM                    DSBS69
070900980123     C                   PARM                    DSACO
071000980123     C                   PARM                    DSIND
071100980123     C                   PARM                    DSCLP
071200980123     C                   PARM                    DSCLS
071300980123     C     O69ERR        IFNE      '1'
071400980123     C     ACOFLG        IFEQ      '*'
071500980123     C                   MOVEL     *BLANKS       §LDRSM
071600980123     C                   ELSE
071700980123     C                   MOVEL     ACORAG        §LDRSM
071800980123     C                   END
071900980123     C                   MOVEL     INDPRV        §LDPRM
072000980123     C                   END
072100960104     C                   MOVEL     VABRSD        §LDRSD
072200960104     C                   MOVEL     VABCAD        §LDCAD
072300960104     C                   MOVEL     VABLOD        §LDLOD
072400960104     C                   MOVEL     VABIND        §LDIND
072500960104     C                   MOVEL     VABPRD        §LDPRD
072600921026     C                   MOVEL     999           §LDCPD
072700921026     C                   MOVEL     *ZEROS        §LDNCP
072800921026     C                   MOVEL     *ZEROS        §LDNCU
072900960105     C     VABVLB        IFGT      0
073000921026     C                   MOVEL     'R'           §LDFVR
073100960105     C                   ELSE
073200960105     C                   CLEAR                   §LDFVR
073300960105     C                   ENDIF
073400960104     C                   Z-ADD     VABVLB        §LDVOL
073500960104     C                   Z-ADD     VABPKB        §LDPKG
073600960104     C                   Z-ADD     VABAAS        §LDAAS
073700960104     C                   Z-ADD     VABMGS        §LDMGS
073800971223     C                   MOVEL     'ITA'         §DETST
073900960104     C     CO2SCD        DOWLE     VABNCA
074000921026     C                   ADD       1             §LDNC1
074100921026     C                   Z-ADD     CO2SCD        §LDNCD
074200921026     C*
074300030626     c                   Call      d90pss
074400921026     C                   PARM                    DSBLD
074500971223     C                   PARM                    DSBLDE
074600921026     C*
074700921026     C                   ADD       1             CO2SCD
074800921026     C*
074900921026     C                   END
075000921026     C*
075100921026     C                   ENDSR
075200921026     C*---------------------------------------------------------------
075300921026     C*   GESLET  GESTIONE STAMPA LETTERA DI VETTURA
075400921026     C*---------------------------------------------------------------
075500921026     C*
075600921026     C     GESLET        BEGSR
075700030626     C                   CLEAR                   Fnlsb5ds
075800941228     C                   MOVEL     'B'           DB0GES
075900941228     C                   MOVEL     'P'           DB0TBO
076000921026     C*
076100960104     C                   Z-ADD     VABLNP        DB0LNP
076200960104     C                   Z-ADD     VABAAS        DB0AAS
076300960104     C                   Z-ADD     VABMGS        DB0MGS
076400960104     C                   Z-ADD     VABNRS        DB0NRS
076500960104     C                   Z-ADD     VABNSP        DB0NSP
076600960104     C                   MOVE      VABCBO        DB0CBO
076700960104     C                   MOVEL     §3ATB1        DB0TB1
076800960104     C                   MOVEL     §3ATB2        DB0TB2
076900960104     C                   MOVEL     §3ADES        DB0DCB
077000960104     C                   Z-ADD     VABLNA        DB0LNA
077100941228     C* PASSO INDIRIZZO COMPLETO DELLA LNA
077200960104     C     VABLNA        CHAIN     AZORG                              30
077300941228     C     *IN30         IFEQ      *OFF
077400941228     C                   MOVEL     ORGFAG        DB0FAG
077500941228     C                   MOVEL     ORGDES        DB0DLA
077600941228     C                   MOVEL     ORGIND        DB0INL
077700941228     C                   MOVEL     ORGLOC        DB0LOL
077800941228     C                   MOVEL     ORGPRO        DB0PRL
077900941228     C                   MOVEL     ORGTEL        DB0TEL
078000160519     C                   MOVEL     ORGdd2        DB0FAX
078100030626     c                   Movel     OrgCpf        Db0Cal
078200941228     C                   ENDIF
078300960104     C                   Z-ADD     VABZNC        DB0ZNC
078400960104     C* MITTENTE
078500960104     C                   MOVEL     VABCCM        DB0CCM
078600941228     C                   MOVEL     ACORAG        DB0RSM
078700941228     C                   MOVEL     INDVIA        DB0INM
078800941228     C                   MOVEL     INDCAE        DB0CAM
078900941228     C                   MOVEL     INDCIT        DB0LOM
079000941228     C                   MOVEL     INDPRV        DB0PRM
079100941228     C                   MOVEL     INDSTA        DB0NZM
079200960104     C                   MOVEL     VABRSD        DB0RSD
079300131212     C                   MOVEL     VABRd2        DB0Rd2
079400960104     C                   MOVEL     VABIND        DB0IND
079500960104     C                   MOVEL     VABLOD        DB0LOD
079600960104     C                   MOVEL     VABPRD        DB0PRD
079700960104     C                   MOVEL     VABCAD        DB0CAD
079800960916     C                   MOVEL     VABNZD        DB0NZD
079900960104     C* FLAG INOLTRO  --> SE FERMO DEPOSITO SEMPRE 'C'
080000110621     c*  ai fini della stampa il campo non serve per cui preferisco non pasarlo in vista
080100110621     c*  dei nuovi tipi inoltro per centro storico
080200960104     C                   MOVEL     VABGC1        DB0GC1
080300960104     C                   MOVEL     VABGC2        DB0GC2
080400960104     C                   MOVEL     VABTSP        DB0TSP
080500941228     C* DECODIFICA TIPO SERVIZIO
080600960104     C     VABTSP        IFNE      ' '
080700941228     C                   MOVEL     '5E'          COD
080800960104     C                   MOVEL(P)  VABTSP        KEY
080900941228     C     KTAB2         CHAIN     TABEL                              30
081000941228     C  N30              MOVEL     TBLUNI        DS5E
081100941228     C   30              CLEAR                   DS5E
081200941228     C                   MOVEL     §5EDES        DB0DTS
081300941228     C                   MOVEL     §5EFST        DB0FST
081400960104     C                   ENDIF
081500941228     C*
081600960104     C                   Z-ADD     VABIAS        DB0IAS
081700960104     C                   MOVEL     VABVAS        DB0VAS
081800960104     C                   MOVEL     VABNAS        DB0NAS
081900960104     C                   Z-ADD     VABNCL        DB0NCL
082000960104     C                   Z-ADD     VABPKB        DB0PKF
082100960104     C                   Z-ADD     VABVLB        DB0VLF
082200960105     C     DB0VLF        IFGT      0
082300941228     C                   MOVEL     'R'           DB0FVF
082400960105     C                   ENDIF
082500960104     C                   Z-ADD     VABRMN        DB0RMN
082600960104     C                   Z-ADD     VABLNP        DB0FLS
082700960104     C                   Z-ADD     VABNCD        DB0NCD
082800960104     C                   Z-ADD     VABNCA        DB0NCA
082900960105     C     DB0NCA        IFEQ      DB0NCD
083000960105     C                   CLEAR                   DB0NCA
083100960105     C                   ENDIF
083200960104     C                   MOVEL     VABXCO        DB0XCO
083300960104     C                   MOVEL     VABNOT        DB0NT1
083400960104     C                   MOVEL     VABNT2        DB0NT2
083500960104     C                   MOVEL     VABFFD        DB0FFD
083600110621     C*                                             c
083700030626     c                   call      D90psl
083800030626     C                   PARM                    Fnlsb5ds
083900921026     C*
084000921026     C                   ENDSR
084100941228     OFNLV22P   T    LRNLR
084200131024**
084300131024A C Q U I S T I
084400131024S   E   D   E
084500131024A  L  T  R  O
