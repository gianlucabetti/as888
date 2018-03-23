000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200010628      ****************************************************************
000300010628      *  RIEPILOGO INDICATORI
000400010628      ****************************************************************
000500010628      * 01 - RECORD ANNULLATO
000600010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
000700010705      * 15 - POSIZIONAMENTO CURSORE SULLA DESCRIZIONE
000800010628      * 28 - ERRORE GENERICO DSPF
000900010628      * 31 - GENERICO
001000010628      * 90 - RIEMISSIONE VIDEATA
001100010628      ****************************************************************
001200010628     FAZORG01L  IF   E           K DISK
001300010628     FTNTBE01L  UF A E           K DISK
001400010628     FTNTB14D   CF   E             WORKSTN
001500010628      *------------------------------------------------------------------------*
001600010628     D KCOD            S                   LIKE(TBECOD)
001700010628     D KKE1            S                   LIKE(TBEKE1)
001800010628     D WDATA           S                   LIKE(V2CDAD)
001900010629     D WDAT1           S                   LIKE(V2CDAD)
002000010628      *   S C H I E R E
002100010629     D MSG             S             78    DIM(7) CTDATA PERRCD(1)              MSG VIDEO
002200010628     D DATOGGI         S              8  0
002300010628
002400010628     D W0140           S             14  0
002500010628     D WDTGIO          S              8  0
002600010628     D DATEU           S              8  0
002700010628     D*
002800010628      *   D S   I N T E R N E / E S T E R N E
002900010628     D WLBDAT          DS                  INZ
003000010628     D  G02DAT                 1      8  0
003100010628     D  G02INV                 9     16  0
003200010628     D  G02ERR                17     17
003300010628     D  G02TGI                18     22  0
003400010628     D*
003500010628     D TIBS69DS      E DS
003600010628     D DS_CNACO      E DS                  EXTNAME(CNACO00F)
003700010628     D DS_CNIND      E DS                  EXTNAME(CNIND00F)
003800010628     D DS_CNCLP      E DS                  EXTNAME(CNCLP00F)
003900010628     D DS_FNCLS      E DS                  EXTNAME(FNCLS00F)
004000010628     D TIBS02DS      E DS
004100010628     D UT§DSE0F      E DS
004200010628     D CNCR80        E DS
004300010628     D DTAA          E DS
004400010628     D*
004500010628     D KPJBA         E DS
004600010628     D*
004700010628     D*------------------------------------------------------------------------*
004800010628     C                   DO        *HIVAL
004900010628     C* Pulizia 1° Videata
005000010628     C                   IF        (*IN28 = *OFF AND *IN90 = *OFF)
005100010628     C                   MOVE      *BLANKS       V1CPRG
005200010628     C                   MOVE      '?'           V1CPRG
005300010628     C                   ENDIF
005400010628     C*
005500010628      * Emissione 1° Videata
005600010628     C                   EXFMT     TNTB141
005700010628      * F3=Fine
005800010628     C   KC              LEAVE
005900010628     C*
006000010628      * Controlli 1° videata
006100010628     C                   SETOFF                                       28
006200010628     C                   EXSR      SR_CONTR01
006300010628     C*
006400010628     C   28              ITER
006500010628     C   90              ITER
006600010628     C*
006700010628     C* F13=Ricerca numero libero
006800010628     C   KM              EXSR      SR_RICERCA
006900010628     C   KM              ITER
007000010628      * Caricamento e pulizia seconda videata
007100010628     C                   EXSR      SR_CARVID
007200010628     C*
007300010628      * Emissione 2° Videata
007400010628     C     RIEMETTO      TAG
007500010628     C                   EXFMT     TNTB142
007600010628     C*
007700010628     C                   EVAL      *IN28 = *OFF
007800010705     C                   EVAL      *IN15 = *OFF
007900010628     C*
008000010628      * F3=Fine
008100010628     C   KC              LEAVE
008200010628     C*
008300010628      * F12=Ritorno
008400010628     C   KL              ITER
008500010628     C*
008600010628     C                   EXSR      SR_CONTR02
008700010628     C   28              GOTO      RIEMETTO
008800010628     C*
008900010628     C* F5=Ripristino
009000010628     C   KE              MOVEL     *BLANKS       WANN              1
009100010628     C*
009200010628     C* F16=Annullamento
009300010628     C   KQ              MOVEL     'A'           WANN              1
009400010628     C*
009500010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
009600010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
009700010629     C                   EXSR      SR_REGISTRA
009800010628     C                   ITER
009900010628     C                   ENDIF
010000010628     C*
010100010628     C                   GOTO      RIEMETTO
010200010628     C*
010300010628     C                   ENDDO
010400010628     C*
010500010628     C                   EVAL      *INLR = *ON
010600010628      **********************************************************************
010700010628      * CONTROLLI VIDEO 1
010800010628      **********************************************************************
010900010628     C     SR_CONTR01    BEGSR
011000010628      * Reset indicatori
011100010628     C                   SETOFF                                       90
011200010628     C     '?'           SCAN      V1CPRG                                 31
011300010628     C     *IN31         IFEQ      *ON
011400010628     C*Se non ho premuto F13 controllo il ? e il campo valorizzato
011500010628     C                   IF        *INKM = *ON
011600010628     C                   GOTO      TAGF13
011700010628     C                   ENDIF
011800010628     C*
011900010628     C                   EVAL      *IN90 = *ON
012000010628     C                   CLEAR                   TIBS02DS
012100010628     C                   MOVEL     'R'           T02MOD
012200010628     C                   MOVEL     KNSIF         T02SIF
012300010628     C                   MOVEL     'TAA'         T02COD
012400010628     C                   CALL      'TIBS02R'
012500010628     C                   PARM                    KPJBA
012600010628     C                   PARM                    TIBS02DS
012700010628     C                   IF        T02ERR = *BLANKS
012800010628     C                   MOVEL     T02KE1        V1CPRG
012900010628     C                   SETON                                        90
013000010628     C                   ELSE
013100010628     C                   SETON                                        28
013200010628     C                   MOVEL     MSG(1)        V1CMSG
013300010628     C                   GOTO      ENDSRCONTR
013400010628     C                   ENDIF
013500010628     C                   ENDIF
013600010628
013700010628      * controllo se campo valorizzato
013800010628     C                   IF        V1CPRG = *ZEROS OR V1CPRG = *BLANKS
013900010628     C                   IF        *INKM = *OFF
014000010628     C                   SETON                                        28
014100010628     C                   MOVEL     MSG(1)        V1CMSG
014200010628     C                   GOTO      ENDSRCONTR
014300010628     C                   ENDIF
014400010628     C                   ENDIF
014500010628     C*
014600010628     C* controllo che contenga solo caratteri numerici
014700010628     C                   IF        %SUBST(V1CPRG:1:1) < '0' OR
014800010628     C                             %SUBST(V1CPRG:2:1) < '0' OR
014900010628     C                             %SUBST(V1CPRG:3:1) < '0'
015000010628     C                   SETON                                        28        controllo P.O.emi.
015100010628     C                   MOVEL     MSG(2)        V1CMSG
015200010628     C                   GOTO      ENDSRCONTR                                   normale
015300010628     C                   ENDIF
015400010628     C*
015500010628     C     TAGF13        TAG
015600010628     C*
015700010628     C* Se premuto F13 il campo non deve essere valorizzato (escludo '?')
015800010628     C                   IF        *INKM = *ON AND *IN31 = *OFF AND
015900010628     C                             V1CPRG <> *BLANKS AND V1CPRG <> *ZEROS
016000010628     C                   SETON                                        28
016100010628     C                   MOVEL     MSG(3)        V1CMSG
016200010628     C                   GOTO      ENDSRCONTR
016300010628     C                   ENDIF
016400010628     C*
016500010628     C     ENDSRCONTR    endsr
016600010628      **********************************************************************
016700010628      * CARICAMENTO SECONDA VIDEATA
016800010628      **********************************************************************
016900010628     C     SR_CARVID     BEGSR
017000010628     C*
017100010628     C                   SETOFF                                       01        ANNULLAMENTO
017200010628     C                   MOVEL     V1CPRG        V2CPRG
017300010628     C                   Z-ADD     *ZEROS        V2CDAD
017400010628     C                   MOVEL     *BLANKS       V2CDES
017500010628      *
017600010628     C                   MOVEL     V1CPRG        KKE1
017700010628     C                   MOVEL     'TAA'         KCOD
017800010628     C     KTNTBE        CHAIN     TNTBE01L                           31
017900010628     C                   IF        *IN31 = *OFF
018000010628     C                   SETON                                        02        VARIAZIONE
018100010628     C                   MOVEL     TBEUNI        DTAA
018200010629     C                   CLEAR                   WLBDAT
018300010629     C                   Z-ADD     §DCCDAD       G02INV
018400010629     C                   MOVEL     '3'           G02ERR
018500010629     C                   CALL      'XSRDA8'
018600010629     C                   PARM                    WLBDAT
018700010629     C                   IF        G02ERR <> '1'
018800010629     C                   MOVEL     G02DAT        V2CDAD
018900010629     C                   ENDIF
019000010628     C                   MOVEL     §DCCDES       V2CDES
019100010629     C                   ELSE
019200010629     C                   SETOFF                                       02
019300010628     C                   ENDIF
019400010628     C*
019500010628     C                   MOVEL     *BLANKS       V2VAIM                         VARIAZ./IMMISSIONE
019600010628     C*
019700010628     C                   IF        *IN02 = *ON                                  VARIAZIONE
019800010628     C                   IF        TBEATB = *BLANKS
019900010628     C                   MOVEL     'VARIAZIONE'  V2VAIM
020000010628     C                   ELSE
020100010628     C                   MOVEL     'ANNULLATO '  V2VAIM
020200010628     C                   SETON                                        01        ANNULLATO
020300010628     C                   ENDIF
020400010628     C                   ELSE                                                   IMMISSIONE
020500010628     C                   MOVEL     'IMMISSIONE'  V2VAIM
020600010628     C                   ENDIF
020700010628     C*
020800010628     C                   ENDSR
020900010628      **********************************************************************
021000010628      * CONTROLLI VIDEO 2
021100010628      **********************************************************************
021200010628     C     Sr_Contr02    begsr
021300010628      * Reset indicatori
021400010628     C                   SETOFF                                       28
021500010628     C*
021600010628     C                   IF        V2CDAD = *ZEROS
021700010628     C                   SETON                                        28
021800010628     C                   MOVEL     MSG(4)        V2CMSG
021900010628     C                   GOTO      ENDSRCONT2
022000010628     C                   ENDIF
022100010628     C*
022200010629     C                   CLEAR                   WLBDAT
022300010628     C                   Z-ADD     V2CDAD        G02DAT
022400010628     C                   MOVEL     *BLANK        G02ERR
022500010628     C                   CALL      'XSRDA8'
022600010628     C                   PARM                    WLBDAT
022700010628     C                   IF        G02ERR = '1'
022800010628     C                   SETON                                        28
022900010628     C                   MOVEL     MSG(6)        V2CMSG
023000010628     C                   GOTO      ENDSRCONT2
023100010629     C                   ELSE
023200010629     C                   MOVEL     G02DAT        V2CDAD
023300010628     C                   ENDIF
023400010628     C                   MOVEL     G02INV        WDATA
023500010628     C*
023600010705     C                   IF        V2CDES = *BLANKS
023700010705     C                   SETON                                        2815
023800010705     C                   MOVEL     MSG(5)        V2CMSG
023900010705     C                   GOTO      ENDSRCONT2
024000010705     C                   ENDIF
024100010705     C*
024200010629     C                   IF        *IN02 = *OFF
024300010629     C                   MOVEL     'TAA'         KCOD
024400010629     C     KCOD          SETLL     TNTBE01L
024500010629     C                   DO        *HIVAL
024600010629     C     KCOD          READE     TNTBE01L
024700010629     C                   IF        %EOF(TNTBE01L)
024800010629     C                   LEAVE
024900010629     C                   ENDIF
025000010629     C                   MOVEL     TBEUNI        WDAT1
025100010629     C                   IF        WDATA <= WDAT1
025200010629     C                   SETON                                        28
025300010629     C                   MOVEL     MSG(7)        V2CMSG
025400010629     C                   GOTO      ENDSRCONT2
025500010629     C                   ENDIF
025600010629     C                   ENDDO
025700010629     C                   ENDIF
025800010629     C*
025900010628     C     ENDSRCONT2    ENDSR
026000010628      **********************************************************************
026100010628      * REGISTRAZIONE DATI IMMESSI
026200010628      **********************************************************************
026300010628     C     SR_REGISTRA   BEGSR
026400010628
026500010628     C                   MOVEL     'TAA'         KCOD
026600010628     C                   MOVEL     V2CPRG        KKE1
026700010628     C     KTNTBE        CHAIN     TNTBE01L                           31
026800010628     C                   IF        *IN31 = *ON
026900010628     C                   MOVE      *BLANKS       TBEATB
027000010628     C                   MOVEL     'TAA'         TBECOD
027100010628     C                   MOVEL     V2CPRG        TBEKE1
027200010628     C                   MOVE      'S'           TBEFTT
027300010628     C                   MOVE      *BLANKS       TBEFTR
027400010629     C                   MOVEL     WDATA         §DCCDAD
027500010628     C                   MOVEL     V2CDES        §DCCDES
027600010628     C                   MOVEL     DTAA          TBEUNI
027700010628     C                   WRITE     TNTBE000
027800010628     C                   ELSE
027900010628     C                   MOVE      'S'           TBEFTT
028000010628     C                   MOVE      *BLANKS       TBEFTR
028100010629     C                   MOVEL     WDATA         §DCCDAD
028200010628     C                   MOVEL     V2CDES        §DCCDES
028300010628     C                   MOVEL     DTAA          TBEUNI
028400010628     C                   MOVE      WANN          TBEATB
028500010628     C                   UPDATE    TNTBE000
028600010628     C                   ENDIF
028700010628     C     ENDREGISTRA   ENDSR
028800010628      **********************************************************************
028900010628      * RICERCA NUMERO LIBERO
029000010628      **********************************************************************
029100010628     C     SR_RICERCA    BEGSR
029200010628     C*
029300010629     C                   Z-ADD     *ZEROS        WPROGR            3 0
029400010628     C                   MOVEL     'TAA'         TBECOD
029500010628     C     TBECOD        SETLL     TNTBE01L
029600010628     C                   DO        *HIVAL
029700010628     C     TBECOD        READE     TNTBE01L                             31
029800010628     C                   IF        *IN31 = *ON
029900010628     C                   ADD       1             WPROGR
030000010628     C                   MOVEL     WPROGR        V1CPRG
030100010628     C                   LEAVE
030200010628     C                   ENDIF
030300010629     C     WPROGR        ADD       1             WPRG              3 0
030400010629     C                   MOVEL     TBEKE1        WCODICE           3 0
030500010628     C*Testo che i codici siano successivi e che non esistano buchi
030600010628     C                   IF        WCODICE <> WPRG
030700010628     C                   MOVEL     WPRG          V1CPRG
030800010628     C                   LEAVE
030900010628     C                   ELSE
031000010628     C                   Z-ADD     WCODICE       WPROGR                         SALVO LA SERIE
031100010628     C                   ITER
031200010628     C                   ENDIF
031300010628     C                   ENDDO
031400010628     C                   SETON                                        90
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
032800010628     C                   MOVEL     REC80         CNCR80
032900010628     C*
033000010628     C     KTNTBE        KLIST
033100010628     C                   KFLD                    KCOD
033200010628     C                   KFLD                    KKE1
033300010628     C*
033400010628     C                   endsr
033500010628      *---------------------------------------------------------------------------------------------
033600010628** MSG  Lungh. 78                                                            *
033700010628Inserire Progressivo
033800010628Il Numero Progressivo accetta solo caratteri numerici
033900010628Non è possibile utilizzare questo tasto funzione col Progressivo valorizzato
034000010628Inserire le Data Decorrenza
034100010628Inserire la Descrizione
034200010628Data Decorrenza errata
034300010629La Data Decorrenza deve essere superiore all'ultima immessa
