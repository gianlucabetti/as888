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
001200010628     FTNTB13D   CF   E             WORKSTN
001300010628      *------------------------------------------------------------------------*
001400010628     D KCOD            S                   LIKE(TBECOD)
001500010628     D KKE1            S                   LIKE(TBEKE1)
001600010628      *   S C H I E R E
001700010628     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG VIDEO
001800010628     D DATOGGI         S              8  0
001900010628
002000010628     D W0140           S             14  0
002100010628     D WDTGIO          S              8  0
002200010628     D DATEU           S              8  0
002300010628     D*
002400010628      *   D S   I N T E R N E / E S T E R N E
002500010628     D WLBDAT          DS                  INZ
002600010628     D  G02DAT                 1      8  0
002700010628     D  G02INV                 9     16  0
002800010628     D  G02ERR                17     17
002900010628     D  G02TGI                18     22  0
003000010628     D*
003100010628     D TIBS69DS      E DS
003200010628     D DS_CNACO      E DS                  EXTNAME(CNACO00F)
003300010628     D DS_CNIND      E DS                  EXTNAME(CNIND00F)
003400010628     D DS_CNCLP      E DS                  EXTNAME(CNCLP00F)
003500010628     D DS_FNCLS      E DS                  EXTNAME(FNCLS00F)
003600010628     D TIBS02DS      E DS
003700010628     D UT§DSE0F      E DS
003800010628     D CNCR80        E DS
003900010628     D DDCC          E DS
004000010628     D*
004100010628     D KPJBA         E DS
004200010628     D*
004300010628     D*------------------------------------------------------------------------*
004400010628     C                   DO        *HIVAL
004500010628     C* Pulizia 1° Videata
004600010628     C                   IF        (*IN28 = *OFF AND *IN90 = *OFF)
004700010628     C                   MOVE      *BLANKS       V1CSER
004800010628     C                   MOVE      '?'           V1CSER
004900010628     C                   ENDIF
005000010628     C*
005100010628      * Emissione 1° Videata
005200010628     C                   EXFMT     TNTB131
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
006300010628     C* F13=Ricerca numero libero
006400010628     C   KM              EXSR      SR_RICERCA
006500010628     C   KM              ITER
006600010628      * Caricamento e pulizia seconda videata
006700010628     C                   EXSR      SR_CARVID
006800010628     C*
006900010628      * Emissione 2° Videata
007000010628     C     RIEMETTO      TAG
007100010628     C                   EXFMT     TNTB132
007200010628     C*
007300010628     C                   EVAL      *IN28 = *OFF
007400010628     C*
007500010628      * F3=Fine
007600010628     C   kc              LEAVE
007700010628     C*
007800010628      * F12=Ritorno
007900010628     C   kl              ITER
008000010628     C*
008100010628     C                   EXSR      SR_CONTR02
008200010628     C   28              GOTO      RIEMETTO
008300010628     C*
008400010628     C* F5=Ripristino
008500010628     C   KE              MOVEL     *BLANKS       WANN              1
008600010628     C*
008700010628     C* F16=Annullamento
008800010628     C   KQ              MOVEL     'A'           WANN              1
008900010628     C*
009000010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
009100010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
009200010628     C                   EXSR      SR_REGISTRA
009300010628     C                   ITER
009400010628     C                   ENDIF
009500010628     C*
009600010628     C                   GOTO      RIEMETTO
009700010628     C*
009800010628     C                   ENDDO
009900010628     C*
010000010628     C                   EVAL      *INLR = *ON
010100010628      **********************************************************************
010200010628      * CONTROLLI VIDEO 1
010300010628      **********************************************************************
010400010628     C     SR_CONTR01    BEGSR
010500010628      * Reset indicatori
010600010628     C                   SETOFF                                       90
010700010628     C     '?'           SCAN      V1CSER                                 31
010800010628     C     *IN31         IFEQ      *ON
010900010628     C*Se non ho premuto F13 controllo il ? e il campo valorizzato
011000010628     C                   IF        *INKM = *ON
011100010628     C                   GOTO      TAGF13
011200010628     C                   ENDIF
011300010628     C*
011400010628     C                   EVAL      *IN90 = *ON
011500010628     C                   CLEAR                   TIBS02DS
011600010628     C                   MOVEL     'R'           T02MOD
011700010628     C                   MOVEL     KNSIF         T02SIF
011800010628     C                   MOVEL     'DCC'         T02COD
011900010628     C                   CALL      'TIBS02R'
012000010628     C                   PARM                    KPJBA
012100010628     C                   PARM                    TIBS02DS
012200010628     C                   IF        T02ERR =  *BLANKS
012300010628     C                   MOVEL     T02KE1        V1CSER
012400010628     C                   SETON                                        90
012500010628     C                   ELSE
012600010628     C                   SETON                                        28
012700010628     C                   MOVEL     MSG(1)        V1CMSG
012800010628     C                   GOTO      ENDSRCONTR
012900010628     C                   ENDIF
013000010628     C                   ENDIF
013100010628
013200010628      * controllo se campo valorizzato
013300010628     C                   IF        V1CSER = *ZEROS OR V1CSER = *BLANKS
013400010628     C                   IF        *INKM = *OFF
013500010628     C                   SETON                                        28
013600010628     C                   MOVEL     MSG(1)        V1CMSG
013700010628     C                   GOTO      ENDSRCONTR
013800010628     C                   ENDIF
013900010628     C                   ENDIF
014000010628     C*
014100010628     C* controllo che contenga solo caratteri numerici
014200010628     C                   IF        %SUBST(V1CSER:1:1) < '0' OR
014300010628     C                             %SUBST(V1CSER:2:1) < '0' OR
014400010628     C                             %SUBST(V1CSER:3:1) < '0' OR
014500010628     C                             %SUBST(V1CSER:4:1) < '0'
014600010628     C                   SETON                                        28        controllo P.O.emi.
014700010628     C                   MOVEL     MSG(2)        V1CMSG
014800010628     C                   GOTO      ENDSRCONTR                                   normale
014900010628     C                   ENDIF
015000010628     C*
015100010628     C     TAGF13        TAG
015200010628     C*
015300010628     C* Se premuto F13 il campo non deve essere valorizzato (escludo '?')
015400010628     C                   IF        *INKM = *ON AND *IN31 = *OFF AND
015500010628     C                             V1CSER <> *BLANKS AND V1CSER <> *ZEROS
015600010628     C                   SETON                                        28
015700010628     C                   MOVEL     MSG(3)        V1CMSG
015800010628     C                   GOTO      ENDSRCONTR
015900010628     C                   ENDIF
016000010628     C*
016100010628     C     ENDSRCONTR    endsr
016200010628      **********************************************************************
016300010628      * CONTROLLI VIDEO 2
016400010628      **********************************************************************
016500010628     C     Sr_Contr02    begsr
016600010628      * Reset indicatori
016700010628     C                   SETOFF                                       28
016800041221     c                   clear                   V2Dksc
016900010628     C*
017000010628     C                   IF        V2CKSC = *ZEROS
017100010628     C                   SETON                                        28
017200010628     C                   MOVEL     MSG(4)        V2CMSG
017300010628     C                   GOTO      ENDSRCONT2
017400010628     C                   ENDIF
017500010628     C*
017600010628     C                   CLEAR                   TIBS69DS
017700010628     C                   Z-ADD     V2CKSC        I69KAC
017800010628     C                   CALL      'TIBS69R'
017900010628     C                   PARM                    TIBS69DS
018000010628     C                   PARM                    DS_CNACO
018100010628     C                   PARM                    DS_CNIND
018200010628     C                   PARM                    DS_CNCLP
018300010628     C                   PARM                    DS_FNCLS
018400010628     C*
018500010628     C                   IF        O69ERR = *BLANKS
018600010628     C                   MOVEL     ACORAG        V2DKSC
018700010628     C                   ELSE
018800010628     C                   SETON                                        28
018900010628     C                   MOVEL     MSG(5)        V2CMSG
019000010628     C                   GOTO      ENDSRCONT2
019100010628     C                   ENDIF
019200010628     C*
019300010628     C     ENDSRCONT2    ENDSR
019400010628      **********************************************************************
019500010628      * CARICAMENTO SECONDA VIDEATA
019600010628      **********************************************************************
019700010628     C     SR_CARVID     BEGSR
019800010628     C*
019900010628     C                   SETOFF                                       01        ANNULLAMENTO
020000010628     C                   MOVEL     V1CSER        V2CSER
020100010628     C                   MOVEL     *ZEROS        V2CKSC
020200010628     C                   MOVEL     *BLANKS       V2DKSC
020300010628      *
020400010628     C                   MOVEL     V1CSER        KKE1
020500010628     C                   MOVEL     'DCC'         KCOD
020600010628     C     KTNTBE        CHAIN     TNTBE01L                           31
020700010628     C                   IF        *IN31 = *OFF
020800010628     C                   SETON                                        02        VARIAZIONE
020900010628     C                   MOVEL     TBEUNI        DDCC
021000010628     C                   MOVEL     §DCCKSC       V2CKSC
021100041221     c                   movel     §DCCksd       V2Dksc
021200010628     C                   ENDIF
021300010628     C*
021400041221     c                   if            V2Cksc <> *zeros
021500041221     c                             and V2Dksc =  *blanks
021600010628     C                   CLEAR                   TIBS69DS
021700010628     C                   Z-ADD     V2CKSC        I69KAC
021800010628     C                   CALL      'TIBS69R'
021900010628     C                   PARM                    TIBS69DS
022000010628     C                   PARM                    DS_CNACO
022100010628     C                   PARM                    DS_CNIND
022200010628     C                   PARM                    DS_CNCLP
022300010628     C                   PARM                    DS_FNCLS
022400010628     C*
022500010628     C                   IF        O69ERR = *BLANKS
022600010628     C                   MOVEL     ACORAG        V2DKSC
022700010628     C                   ENDIF
022800041221     c                   endif
022900010628     C*
023000010628     C                   MOVEL     *BLANKS       V2VAIM                         VARIAZ./IMMISSIONE
023100010628     C*
023200010628     C                   IF        *IN02 = *ON                                  VARIAZIONE
023300010628     C                   IF        TBEATB = *BLANKS
023400010628     C                   MOVEL     'VARIAZIONE'  V2VAIM
023500010628     C                   ELSE
023600010628     C                   MOVEL     'ANNULLATO '  V2VAIM
023700010628     C                   SETON                                        01        ANNULLATO
023800010628     C                   ENDIF
023900010628     C                   ELSE                                                   IMMISSIONE
024000010628     C                   MOVEL     'IMMISSIONE'  V2VAIM
024100010628     C                   ENDIF
024200010628     C*
024300010628     C                   ENDSR
024400010628      **********************************************************************
024500010628      * REGISTRAZIONE DATI IMMESSI
024600010628      **********************************************************************
024700010628     C     SR_REGISTRA   BEGSR
024800010628
024900041221     c                   clear                   dDCC
025000041221     C                   MOVEL     V2CKSC        §DCCKSC
025100041221     c                   movel     V2Dksc        §DCCksd
025200010628     C                   MOVEL     'DCC'         KCOD
025300010628     C                   MOVEL     V2CSER        KKE1
025400010628     C     KTNTBE        CHAIN     TNTBE01L                           31
025500010628     C                   IF        *IN31 = *ON
025600010628     C                   MOVE      *BLANKS       TBEATB
025700010628     C                   MOVEL     'DCC'         TBECOD
025800010628     C                   MOVEL     V2CSER        TBEKE1
025900010628     C                   MOVE      'S'           TBEFTT
026000010628     C                   MOVE      *BLANKS       TBEFTR
026100010628     C                   MOVEL     DDCC          TBEUNI
026200010628     C                   WRITE     TNTBE000
026300010628     C                   ELSE
026400010628     C                   MOVE      'S'           TBEFTT
026500010628     C                   MOVE      *BLANKS       TBEFTR
026600010628     C                   MOVEL     DDCC          TBEUNI
026700010628     C                   MOVE      WANN          TBEATB
026800010628     C                   UPDATE    TNTBE000
026900010628     C                   ENDIF
027000010628     C     ENDREGISTRA   ENDSR
027100010628      **********************************************************************
027200010628      * RICERCA NUMERO LIBERO
027300010628      **********************************************************************
027400010628     C     SR_RICERCA    BEGSR
027500010628     C*
027600010628     C                   Z-ADD     *ZEROS        WSERIE            4 0
027700010628     C                   MOVEL     'DCC'         TBECOD
027800010628     C     TBECOD        SETLL     TNTBE01L
027900010628     C                   DO        *HIVAL
028000010628     C     TBECOD        READE     TNTBE01L                             31
028100010628     C                   IF        *IN31 = *ON
028200010628     C                   ADD       1             WSERIE
028300010628     C                   MOVEL     WSERIE        V1CSER
028400010628     C                   LEAVE
028500010628     C                   ENDIF
028600010628     C     WSERIE        ADD       1             WSER              4 0
028700010628     C                   MOVEL     TBEKE1        WCODICE           4 0
028800010628     C*Testo che i codici siano successivi e che non esistano buchi
028900010628     C                   IF        WCODICE <> WSER
029000010628     C                   MOVEL     WSER          V1CSER
029100010628     C                   LEAVE
029200010628     C                   ELSE
029300010628     C                   Z-ADD     WCODICE       WSERIE                         SALVO LA SERIE
029400010628     C                   ITER
029500010628     C                   ENDIF
029600010628     C                   ENDDO
029700010628     C                   SETON                                        90
029800010628     C                   ENDSR
029900010628      *****************************************************************
030000010628      * ROUTINE INIZIALE
030100010628      *****************************************************************
030200010628     C     *INZSR        BEGSR
030300010628      *
030400010628     C     *ENTRY        PLIST
030500010628     C                   PARM                    KPJBA
030600010628     C*
030700010628     C                   Z-ADD     1             CODUT
030800010628     C                   CALL      'X§PARUT'
030900010628     C                   PARM                    UT§DSE0F
031000010628     C                   MOVEL     RAGUT         RSUT
031100010628     C                   MOVEL     REC80         CNCR80
031200010628     C*
031300010628      * reperisco data e ora
031400010628     C                   TIME                    W0140
031500010628      * UDATE IN GGMMAAAA
031600010628     C                   MOVE      W0140         WDTGIO
031700010628      * UDATE IN AAAAMMGG
031800010628     C                   Z-ADD     WDTGIO        G02DAT
031900010628     C                   MOVEL     *BLANK        G02ERR
032000010628     C                   CALL      'XSRDA8'
032100010628     C                   PARM                    WLBDAT
032200010628     C                   MOVEL     G02INV        DATEU
032300010628     C                   MOVEL     DATEU         DATOGGI
032400010628     C*
032500010628     C     KTNTBE        KLIST
032600010628     C                   KFLD                    KCOD
032700010628     C                   KFLD                    KKE1
032800010628     C*
032900010628     C                   endsr
033000010628      *---------------------------------------------------------------------------------------------
033100010628** MSG  Lungh. 78                                                            *
033200010628Inserire Numero Serie
033300010628La Serie accetta solo caratteri numerici
033400010628Non è possibile utilizzare questo tasto funzione con la Serie valorizzata
033500010628Inserire il Codice Cliente
033600010628Codice Cliente inesistente
