000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200010628      ****************************************************************
000300010628      *  RIEPILOGO INDICATORI
000400010628      ****************************************************************
000500010628      * 01 - RECORD ANNULLATO
000600010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
000700010628      * 28 - ERRORE GENERICO DSPF
000800010628      * 31 - GENERICO
000900010628      * 90 - RIEMISSIONE VIDEATA
001000010628      ****************************************************************
001100010628     FTNTBE01L  UF A E           K DISK
001200040503     FTNTB43D   CF   E             WORKSTN
001300010628      *------------------------------------------------------------------------*
001400040503     D KCOD            S                   LIKE(TBECOD) inz('PSC')
001500010628     D KKE1            S                   LIKE(TBEKE1)
001600040503     D WDATAD          S                   LIKE(V2CDAD)
001700040504
001800040504     D DATOGGI         S              8  0
001900040504
002000040504     D W0140           S             14  0
002100040504     D WDTGIO          S              8  0
002200040504     D DATEU           S              8  0
002300040504
002400040504     D Wprogr          s              9  0
002500040504     D Wdecpre         s              8  0
002600040504
002700010628      *   S C H I E R E
002800040504     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             MSG VIDEO
002900010628     D*
003000010628      *   D S   I N T E R N E / E S T E R N E
003100010628     D WLBDAT          DS                  INZ
003200010628     D  G02DAT                 1      8  0
003300010628     D  G02INV                 9     16  0
003400010628     D  G02ERR                17     17
003500010628     D  G02TGI                18     22  0
003600010628     D*
003700010628     D TIBS02DS      E DS
003800010628     D UT§DSE0F      E DS
003900040503     D DPSC          E DS
004000010628     D*
004100010628     D KPJBA         E DS
004200010628     D*
004300010628     D*------------------------------------------------------------------------*
004400010628     C                   DO        *HIVAL
004500010628     C* Pulizia 1° Videata
004600010628     C                   IF        (*IN28 = *OFF AND *IN90 = *OFF)
004700010628     C                   MOVE      *BLANKS       V1CPRG
004800010628     C                   MOVE      '?'           V1CPRG
004900010628     C                   ENDIF
005000010628     C*
005100010628      * Emissione 1° Videata
005200040503     C                   EXFMT     TNTB431
005300010628      * F3=Fine
005400010628     C   KC              LEAVE
005500010628     C*
005600010628      * Controlli 1° videata
005700010628     C                   SETOFF                                       28
005800010628     C                   EXSR      SR_CONTR01
005900010628     C*
006000010628     C   28              ITER
006100010628     C   90              ITER
006200010628     C*
006300040504     C* F10=IMMISSIONE
006400040504     C   KJ              EXSR      SR_RICERCA
006500010628      * Caricamento e pulizia seconda videata
006600010628     C                   EXSR      SR_CARVID
006700010628     C*
006800010628      * Emissione 2° Videata
006900010628     C     RIEMETTO      TAG
007000040504     C                   EXFMT     TNTB432
007100010628     C*
007200010628     C                   EVAL      *IN28 = *OFF
007300010705     C                   EVAL      *IN15 = *OFF
007400010628     C*
007500010628      * F3=Fine
007600010628     C   KC              LEAVE
007700010628     C*
007800010628      * F12=Ritorno
007900010628     C   KL              ITER
008000010628     C*
008100010628     C                   EXSR      SR_CONTR02
008200010628     C   28              GOTO      RIEMETTO
008300010628     C*
008400010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
008500010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
008600010629     C                   EXSR      SR_REGISTRA
008700010628     C                   ITER
008800010628     C                   ENDIF
008900010628     C*
009000010628     C                   GOTO      RIEMETTO
009100010628     C*
009200010628     C                   ENDDO
009300010628     C*
009400010628     C                   EVAL      *INLR = *ON
009500010628      **********************************************************************
009600010628      * CONTROLLI VIDEO 1
009700010628      **********************************************************************
009800010628     C     SR_CONTR01    BEGSR
009900010628      * Reset indicatori
010000010628     C                   SETOFF                                       90
010100010628     C     '?'           SCAN      V1CPRG                                 31
010200010628     C     *IN31         IFEQ      *ON
010300040504     C*Se non ho premuto F10 controllo il ? e il campo valorizzato
010400040504     C                   IF        *INKJ = *ON
010500010628     C                   GOTO      TAGF13
010600010628     C                   ENDIF
010700010628     C*
010800010628     C                   EVAL      *IN90 = *ON
010900010628     C                   CLEAR                   TIBS02DS
011000010628     C                   MOVEL     'R'           T02MOD
011100010628     C                   MOVEL     KNSIF         T02SIF
011200040503     C                   MOVEL     'PSC'         T02COD
011300010628     C                   CALL      'TIBS02R'
011400010628     C                   PARM                    KPJBA
011500010628     C                   PARM                    TIBS02DS
011600010628     C                   IF        T02ERR = *BLANKS
011700010628     C                   MOVEL     T02KE1        V1CPRG
011800010628     C                   SETON                                        90
011900010628     C                   ELSE
012000010628     C                   SETON                                        28
012100010628     C                   MOVEL     MSG(1)        V1CMSG
012200010628     C                   GOTO      ENDSRCONTR
012300010628     C                   ENDIF
012400010628     C                   ENDIF
012500010628
012600010628      * controllo se campo valorizzato
012700010628     C                   IF        V1CPRG = *ZEROS OR V1CPRG = *BLANKS
012800040504     C                   IF        *INKJ = *OFF
012900010628     C                   SETON                                        28
013000010628     C                   MOVEL     MSG(1)        V1CMSG
013100010628     C                   GOTO      ENDSRCONTR
013200010628     C                   ENDIF
013300010628     C                   ENDIF
013400010628     C*
013500010628     C* controllo che contenga solo caratteri numerici
013600010628     C                   IF        %SUBST(V1CPRG:1:1) < '0' OR
013700010628     C                             %SUBST(V1CPRG:2:1) < '0' OR
013800040503     C                             %SUBST(V1CPRG:3:1) < '0' OR
013900040503     C                             %SUBST(V1CPRG:4:1) < '0' OR
014000040503     C                             %SUBST(V1CPRG:5:1) < '0' OR
014100040503     C                             %SUBST(V1CPRG:6:1) < '0' OR
014200040503     C                             %SUBST(V1CPRG:7:1) < '0' OR
014300040503     C                             %SUBST(V1CPRG:8:1) < '0' OR
014400040503     C                             %SUBST(V1CPRG:9:1) < '0'
014500040503     C                   SETON                                        28
014600010628     C                   MOVEL     MSG(2)        V1CMSG
014700010628     C                   GOTO      ENDSRCONTR                                   normale
014800010628     C                   ENDIF
014900010628     C*
015000010628     C     TAGF13        TAG
015100010628     C*
015200040504     C* Se premuto F10 il campo non deve essere valorizzato (escludo '?')
015300040504     C                   IF        *INKJ = *ON AND *IN31 = *OFF AND
015400010628     C                             V1CPRG <> *BLANKS AND V1CPRG <> *ZEROS
015500010628     C                   SETON                                        28
015600010628     C                   MOVEL     MSG(3)        V1CMSG
015700010628     C                   GOTO      ENDSRCONTR
015800010628     C                   ENDIF
015900040504      * se inserito numero progressivo deve esistere il record altrimenti errore
016000040504     c                   If        *inkj = *off
016100040504     C                   MOVEL(p)  V1CPRG        KKE1
016200040504     C     KTNTBE        CHAIN     TNTBE01L
016300040504     C                   IF        not %found(tntbe01l)
016400040504     C                   SETON                                        28
016500040504     C                   MOVEL     MSG(10)       V1CMSG
016600040504     C                   GOTO      ENDSRCONTR
016700040504     C                   ENDIF
016800040504     C                   ENDIF
016900040504     c
017000010628     C*
017100010628     C     ENDSRCONTR    endsr
017200010628      **********************************************************************
017300010628      * CARICAMENTO SECONDA VIDEATA
017400010628      **********************************************************************
017500010628     C     SR_CARVID     BEGSR
017600010628     C*
017700040504     C                   SETOFF                                       01
017800010628     C                   MOVEL     V1CPRG        V2CPRG
017900010628     C                   Z-ADD     *ZEROS        V2CDAD
018000040503     C                   Z-ADD     *ZEROS        V2CPER
018100010628      *
018200040504     C                   IF        not *inkj
018300010628     C                   SETON                                        02        VARIAZIONE
018400040503      * DATA DECORRENZA
018500040503     C                   MOVEL     TBEUNI        DPSC
018600010629     C                   CLEAR                   WLBDAT
018700040503     C                   Z-ADD     §pscDAD       G02INV
018800010629     C                   MOVEL     '3'           G02ERR
018900010629     C                   CALL      'XSRDA8'
019000010629     C                   PARM                    WLBDAT
019100010629     C                   IF        G02ERR <> '1'
019200010629     C                   MOVEL     G02DAT        V2CDAD
019300010629     C                   ENDIF
019400040504      * percentuale
019500040503     C                   z-add     §pscper       V2Cper
019600040504      * cerco il progressivo precedente per recuperare la data decorrenza
019700040504     c                   movel     v2cprg        wprogr
019800040504     c                   sub       1             wprogr
019900040504     c                   movel(p)  wprogr        kke1
020000040504     c     ktntbe        chain     tntbe01l
020100040504     c                   if        %found(tntbe01l) and tbeatb = ' '
020200040504     c                   movel     tbeuni        dpsc
020300040504     c                   z-add     §pscdad       wdecpre
020400040504     c                   else
020500040504     c                   clear                   wdecpre
020600040504     c                   endif
020700040504      *
020800010629     C                   ELSE
020900040504     C                   SETOFF                                       02        IMMISSIONE
021000010628     C                   ENDIF
021100010628     C*
021200010628     C                   MOVEL     *BLANKS       V2VAIM                         VARIAZ./IMMISSIONE
021300010628     C*
021400010628     C                   IF        *IN02 = *ON                                  VARIAZIONE
021500010628     C                   IF        TBEATB = *BLANKS
021600010628     C                   MOVEL     'VARIAZIONE'  V2VAIM
021700010628     C                   ELSE
021800010628     C                   MOVEL     'ANNULLATO '  V2VAIM
021900010628     C                   SETON                                        01        ANNULLATO
022000010628     C                   ENDIF
022100010628     C                   ELSE                                                   IMMISSIONE
022200010628     C                   MOVEL     'IMMISSIONE'  V2VAIM
022300010628     C                   ENDIF
022400010628     C*
022500010628     C                   ENDSR
022600010628      **********************************************************************
022700010628      * CONTROLLI VIDEO 2
022800010628      **********************************************************************
022900010628     C     Sr_Contr02    begsr
023000010628      * Reset indicatori
023100040503     C                   SETOFF                                       284041
023200040503      * Data decorrenza
023300010628     C                   IF        V2CDAD = *ZEROS
023400040503     C                   SETON                                        2840
023500010628     C                   MOVEL     MSG(4)        V2CMSG
023600010628     C                   GOTO      ENDSRCONT2
023700010628     C                   ENDIF
023800010628     C*
023900010629     C                   CLEAR                   WLBDAT
024000010628     C                   Z-ADD     V2CDAD        G02DAT
024100010628     C                   MOVEL     *BLANK        G02ERR
024200010628     C                   CALL      'XSRDA8'
024300010628     C                   PARM                    WLBDAT
024400010628     C                   IF        G02ERR = '1'
024500040503     C                   SETON                                        2840
024600010628     C                   MOVEL     MSG(6)        V2CMSG
024700010628     C                   GOTO      ENDSRCONT2
024800010629     C                   ELSE
024900010629     C                   MOVEL     G02DAT        V2CDAD
025000010628     C                   ENDIF
025100040503     C                   MOVEL     G02INV        WDATAD
025200040504      * Controllo la data con quella precedente
025300040504     c                   if        wdatad < wdecpre
025400040504     C                   SETON                                        2840
025500040503     C                   MOVEL     MSG(8)        V2CMSG
025600040503     C                   GOTO      ENDSRCONT2
025700040503     c                   endif
025800010628     C*
025900040503     C                   IF        V2Cper = *zeros
026000040504     C                   SETON                                        2841
026100040503     C                   MOVEL     MSG(9)        V2CMSG
026200010705     C                   GOTO      ENDSRCONT2
026300010705     C                   ENDIF
026400010629     C*
026500010628     C     ENDSRCONT2    ENDSR
026600010628      **********************************************************************
026700010628      * REGISTRAZIONE DATI IMMESSI
026800010628      **********************************************************************
026900010628     C     SR_REGISTRA   BEGSR
027000010628
027100040504     C                   MOVEL(p)  V2CPRG        KKE1
027200010628     C     KTNTBE        CHAIN     TNTBE01L                           31
027300040504
027400040504     C                   z-add     WDATAD        §PSCDAD
027500040504     C                   z-add     V2Cper        §pscper
027600040504     C                   MOVEL     DPSC          TBEUNI
027700040504     C                   MOVE      'S'           TBEFTT
027800040504     C                   MOVE      *BLANKS       TBEFTR
027900040504      * immissione
028000010628     C                   IF        *IN31 = *ON
028100010628     C                   MOVE      *BLANKS       TBEATB
028200040504     C                   MOVEL     'PSC'         TBECOD
028300010628     C                   MOVEL     V2CPRG        TBEKE1
028400010628     C                   WRITE     TNTBE000
028500010628     C                   ELSE
028600040504      * variazione
028700010628     C                   UPDATE    TNTBE000
028800010628     C                   ENDIF
028900040504
029000010628     C     ENDREGISTRA   ENDSR
029100010628      **********************************************************************
029200010628      * RICERCA NUMERO LIBERO
029300010628      **********************************************************************
029400010628     C     SR_RICERCA    BEGSR
029500010628     C*
029600040504     C                   clear                   wprogr
029700040504     C                   clear                   wdecpre
029800040504     c                   eval      kke1 = *all'9'
029900040504
030000040504     C     ktntbe        setgt     TNTBE01L
030100040504     C     kcod          readpe    TNTBE01L
030200040504
030300040504     c                   If        %eof(tntbe01l)
030400040504     C                   z-add     1             WPROGR
030500040504     c                   z-add     *zeros        wdecpre
030600040504     c                   else
030700040504     C                   movel     TBEKE1        wprogr
030800040504     c                   add       1             wprogr
030900040504     c                   movel     tbeuni        DPSC
031000040504     c                   z-add     §pscdad       wdecpre
031100040504     c                   endif
031200040504
031300040504     c                   move      wprogr        v1cprg
031400040504
031500010628     C                   ENDSR
031600010628      *****************************************************************
031700010628      * ROUTINE INIZIALE
031800010628      *****************************************************************
031900010628     C     *INZSR        BEGSR
032000010628      *
032100010628     C     *ENTRY        PLIST
032200010628     C                   PARM                    KPJBA
032300010628     C*
032400010628     C                   Z-ADD     1             CODUT
032500010628     C                   CALL      'X§PARUT'
032600010628     C                   PARM                    UT§DSE0F
032700010628     C                   MOVEL     RAGUT         RSUT
032800010628     C*
032900010628     C     KTNTBE        KLIST
033000010628     C                   KFLD                    KCOD
033100010628     C                   KFLD                    KKE1
033200010628     C*
033300010628     C                   endsr
033400010628      *---------------------------------------------------------------------------------------------
033500010628** MSG  Lungh. 78                                                            *
033600040504Inserire Progressivo                                                          1
033700040504Il Numero Progressivo accetta solo caratteri numerici                         2
033800040504Non è possibile utilizzare questo tasto funzione col Progressivo valorizzato  3
033900040504Inserire le Data Decorrenza                                                   4
034000040504                                                                              5
034100040504Data errata                                                                   6
034200040504                                                                              7
034300040504Data decorrenza minore della data decorrenza progressivo precedente           8
034400040504Inserire la percentuale di supplemento Carburante                             9
034500040504Progressivo inesistente fare F10 per immettere un nuovo progressivo           10
