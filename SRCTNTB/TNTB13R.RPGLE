000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200110803      ****************************************************************
000300110803
000400110803      *?  ATTENZIONE!!  ?
000500110803      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000600110803      *?    'Interrogazione abilitazioni clienti'              ?
000700110803      *?    In caso di aggiunta/modifica campi alla tabella    ?
000800110803      *?    verificare anche relativo pgm di interrogazione    ?
000900110803      *?    per le abilitazioni clienti --> TNTB131R.          ?
001000110803
001100010628      ****************************************************************
001200010628      *  RIEPILOGO INDICATORI
001300010628      ****************************************************************
001400010628      * 01 - RECORD ANNULLATO
001500010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
001600010628      * 28 - ERRORE GENERICO DSPF
001700010628      * 31 - GENERICO
001800010628      * 90 - RIEMISSIONE VIDEATA
001900010628      ****************************************************************
002000010628     FTNTBE01L  UF A E           K DISK
002100010628     FTNTB13D   CF   E             WORKSTN
002200010628      *------------------------------------------------------------------------*
002300010628     D KCOD            S                   LIKE(TBECOD)
002400010628     D KKE1            S                   LIKE(TBEKE1)
002500010628      *   S C H I E R E
002600010628     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG VIDEO
002700010628     D DATOGGI         S              8  0
002800010628
002900010628     D W0140           S             14  0
003000010628     D WDTGIO          S              8  0
003100010628     D DATEU           S              8  0
003200010628     D*
003300010628      *   D S   I N T E R N E / E S T E R N E
003400010628     D WLBDAT          DS                  INZ
003500010628     D  G02DAT                 1      8  0
003600010628     D  G02INV                 9     16  0
003700010628     D  G02ERR                17     17
003800010628     D  G02TGI                18     22  0
003900010628     D*
004000010628     D TIBS69DS      E DS
004100010628     D DS_CNACO      E DS                  EXTNAME(CNACO00F)
004200010628     D DS_CNIND      E DS                  EXTNAME(CNIND00F)
004300010628     D DS_CNCLP      E DS                  EXTNAME(CNCLP00F)
004400010628     D DS_FNCLS      E DS                  EXTNAME(FNCLS00F)
004500010628     D TIBS02DS      E DS
004600010628     D UT§DSE0F      E DS
004700010628     D CNCR80        E DS
004800010628     D DDCC          E DS
004900010628     D*
005000010628     D KPJBA         E DS
005100010628     D*
005200010628     D*------------------------------------------------------------------------*
005300010628     C                   DO        *HIVAL
005400010628     C* Pulizia 1° Videata
005500010628     C                   IF        (*IN28 = *OFF AND *IN90 = *OFF)
005600010628     C                   MOVE      *BLANKS       V1CSER
005700010628     C                   MOVE      '?'           V1CSER
005800010628     C                   ENDIF
005900010628     C*
006000010628      * Emissione 1° Videata
006100010628     C                   EXFMT     TNTB131
006200010628      * F3=Fine
006300010628     C   KC              LEAVE
006400010628     C*
006500010628      * Controlli 1° videata
006600010628     C                   SETOFF                                       28
006700010628     C                   EXSR      SR_CONTR01
006800010628     C*
006900010628     C   28              ITER
007000010628     C   90              ITER
007100010628     C*
007200010628     C* F13=Ricerca numero libero
007300010628     C   KM              EXSR      SR_RICERCA
007400010628     C   KM              ITER
007500010628      * Caricamento e pulizia seconda videata
007600010628     C                   EXSR      SR_CARVID
007700010628     C*
007800010628      * Emissione 2° Videata
007900010628     C     RIEMETTO      TAG
008000010628     C                   EXFMT     TNTB132
008100010628     C*
008200010628     C                   EVAL      *IN28 = *OFF
008300010628     C*
008400010628      * F3=Fine
008500010628     C   kc              LEAVE
008600010628     C*
008700010628      * F12=Ritorno
008800010628     C   kl              ITER
008900010628     C*
009000010628     C                   EXSR      SR_CONTR02
009100010628     C   28              GOTO      RIEMETTO
009200010628     C*
009300010628     C* F5=Ripristino
009400010628     C   KE              MOVEL     *BLANKS       WANN              1
009500010628     C*
009600010628     C* F16=Annullamento
009700010628     C   KQ              MOVEL     'A'           WANN              1
009800010628     C*
009900010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
010000010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
010100010628     C                   EXSR      SR_REGISTRA
010200010628     C                   ITER
010300010628     C                   ENDIF
010400010628     C*
010500010628     C                   GOTO      RIEMETTO
010600010628     C*
010700010628     C                   ENDDO
010800010628     C*
010900010628     C                   EVAL      *INLR = *ON
011000010628      **********************************************************************
011100010628      * CONTROLLI VIDEO 1
011200010628      **********************************************************************
011300010628     C     SR_CONTR01    BEGSR
011400010628      * Reset indicatori
011500010628     C                   SETOFF                                       90
011600010628     C     '?'           SCAN      V1CSER                                 31
011700010628     C     *IN31         IFEQ      *ON
011800010628     C*Se non ho premuto F13 controllo il ? e il campo valorizzato
011900010628     C                   IF        *INKM = *ON
012000010628     C                   GOTO      TAGF13
012100010628     C                   ENDIF
012200010628     C*
012300010628     C                   EVAL      *IN90 = *ON
012400010628     C                   CLEAR                   TIBS02DS
012500010628     C                   MOVEL     'R'           T02MOD
012600010628     C                   MOVEL     KNSIF         T02SIF
012700010628     C                   MOVEL     'DCC'         T02COD
012800010628     C                   CALL      'TIBS02R'
012900010628     C                   PARM                    KPJBA
013000010628     C                   PARM                    TIBS02DS
013100010628     C                   IF        T02ERR =  *BLANKS
013200010628     C                   MOVEL     T02KE1        V1CSER
013300010628     C                   SETON                                        90
013400010628     C                   ELSE
013500010628     C                   SETON                                        28
013600010628     C                   MOVEL     MSG(1)        V1CMSG
013700010628     C                   GOTO      ENDSRCONTR
013800010628     C                   ENDIF
013900010628     C                   ENDIF
014000010628
014100010628      * controllo se campo valorizzato
014200010628     C                   IF        V1CSER = *ZEROS OR V1CSER = *BLANKS
014300010628     C                   IF        *INKM = *OFF
014400010628     C                   SETON                                        28
014500010628     C                   MOVEL     MSG(1)        V1CMSG
014600010628     C                   GOTO      ENDSRCONTR
014700010628     C                   ENDIF
014800010628     C                   ENDIF
014900010628     C*
015000010628     C* controllo che contenga solo caratteri numerici
015100010628     C                   IF        %SUBST(V1CSER:1:1) < '0' OR
015200010628     C                             %SUBST(V1CSER:2:1) < '0' OR
015300010628     C                             %SUBST(V1CSER:3:1) < '0' OR
015400010628     C                             %SUBST(V1CSER:4:1) < '0'
015500010628     C                   SETON                                        28        controllo P.O.emi.
015600010628     C                   MOVEL     MSG(2)        V1CMSG
015700010628     C                   GOTO      ENDSRCONTR                                   normale
015800010628     C                   ENDIF
015900010628     C*
016000010628     C     TAGF13        TAG
016100010628     C*
016200010628     C* Se premuto F13 il campo non deve essere valorizzato (escludo '?')
016300010628     C                   IF        *INKM = *ON AND *IN31 = *OFF AND
016400010628     C                             V1CSER <> *BLANKS AND V1CSER <> *ZEROS
016500010628     C                   SETON                                        28
016600010628     C                   MOVEL     MSG(3)        V1CMSG
016700010628     C                   GOTO      ENDSRCONTR
016800010628     C                   ENDIF
016900010628     C*
017000010628     C     ENDSRCONTR    endsr
017100010628      **********************************************************************
017200010628      * CONTROLLI VIDEO 2
017300010628      **********************************************************************
017400010628     C     Sr_Contr02    begsr
017500010628      * Reset indicatori
017600010628     C                   SETOFF                                       28
017700041221     c                   clear                   V2Dksc
017800010628     C*
017900010628     C                   IF        V2CKSC = *ZEROS
018000010628     C                   SETON                                        28
018100010628     C                   MOVEL     MSG(4)        V2CMSG
018200010628     C                   GOTO      ENDSRCONT2
018300010628     C                   ENDIF
018400010628     C*
018500010628     C                   CLEAR                   TIBS69DS
018600010628     C                   Z-ADD     V2CKSC        I69KAC
018700010628     C                   CALL      'TIBS69R'
018800010628     C                   PARM                    TIBS69DS
018900010628     C                   PARM                    DS_CNACO
019000010628     C                   PARM                    DS_CNIND
019100010628     C                   PARM                    DS_CNCLP
019200010628     C                   PARM                    DS_FNCLS
019300010628     C*
019400010628     C                   IF        O69ERR = *BLANKS
019500010628     C                   MOVEL     ACORAG        V2DKSC
019600010628     C                   ELSE
019700010628     C                   SETON                                        28
019800010628     C                   MOVEL     MSG(5)        V2CMSG
019900010628     C                   GOTO      ENDSRCONT2
020000010628     C                   ENDIF
020100010628     C*
020200010628     C     ENDSRCONT2    ENDSR
020300010628      **********************************************************************
020400010628      * CARICAMENTO SECONDA VIDEATA
020500010628      **********************************************************************
020600010628     C     SR_CARVID     BEGSR
020700010628     C*
020800010628     C                   SETOFF                                       01        ANNULLAMENTO
020900010628     C                   MOVEL     V1CSER        V2CSER
021000010628     C                   MOVEL     *ZEROS        V2CKSC
021100010628     C                   MOVEL     *BLANKS       V2DKSC
021200010628      *
021300010628     C                   MOVEL     V1CSER        KKE1
021400010628     C                   MOVEL     'DCC'         KCOD
021500010628     C     KTNTBE        CHAIN     TNTBE01L                           31
021600010628     C                   IF        *IN31 = *OFF
021700010628     C                   SETON                                        02        VARIAZIONE
021800010628     C                   MOVEL     TBEUNI        DDCC
021900010628     C                   MOVEL     §DCCKSC       V2CKSC
022000041221     c                   movel     §DCCksd       V2Dksc
022100010628     C                   ENDIF
022200010628     C*
022300041221     c                   if            V2Cksc <> *zeros
022400041221     c                             and V2Dksc =  *blanks
022500010628     C                   CLEAR                   TIBS69DS
022600010628     C                   Z-ADD     V2CKSC        I69KAC
022700010628     C                   CALL      'TIBS69R'
022800010628     C                   PARM                    TIBS69DS
022900010628     C                   PARM                    DS_CNACO
023000010628     C                   PARM                    DS_CNIND
023100010628     C                   PARM                    DS_CNCLP
023200010628     C                   PARM                    DS_FNCLS
023300010628     C*
023400010628     C                   IF        O69ERR = *BLANKS
023500010628     C                   MOVEL     ACORAG        V2DKSC
023600010628     C                   ENDIF
023700041221     c                   endif
023800010628     C*
023900010628     C                   MOVEL     *BLANKS       V2VAIM                         VARIAZ./IMMISSIONE
024000010628     C*
024100010628     C                   IF        *IN02 = *ON                                  VARIAZIONE
024200010628     C                   IF        TBEATB = *BLANKS
024300010628     C                   MOVEL     'VARIAZIONE'  V2VAIM
024400010628     C                   ELSE
024500010628     C                   MOVEL     'ANNULLATO '  V2VAIM
024600010628     C                   SETON                                        01        ANNULLATO
024700010628     C                   ENDIF
024800010628     C                   ELSE                                                   IMMISSIONE
024900010628     C                   MOVEL     'IMMISSIONE'  V2VAIM
025000010628     C                   ENDIF
025100010628     C*
025200010628     C                   ENDSR
025300010628      **********************************************************************
025400010628      * REGISTRAZIONE DATI IMMESSI
025500010628      **********************************************************************
025600010628     C     SR_REGISTRA   BEGSR
025700010628
025800041221     c                   clear                   dDCC
025900041221     C                   MOVEL     V2CKSC        §DCCKSC
026000041221     c                   movel     V2Dksc        §DCCksd
026100010628     C                   MOVEL     'DCC'         KCOD
026200010628     C                   MOVEL     V2CSER        KKE1
026300010628     C     KTNTBE        CHAIN     TNTBE01L                           31
026400010628     C                   IF        *IN31 = *ON
026500010628     C                   MOVE      *BLANKS       TBEATB
026600010628     C                   MOVEL     'DCC'         TBECOD
026700010628     C                   MOVEL     V2CSER        TBEKE1
026800010628     C                   MOVE      'S'           TBEFTT
026900010628     C                   MOVE      *BLANKS       TBEFTR
027000010628     C                   MOVEL     DDCC          TBEUNI
027100010628     C                   WRITE     TNTBE000
027200010628     C                   ELSE
027300010628     C                   MOVE      'S'           TBEFTT
027400010628     C                   MOVE      *BLANKS       TBEFTR
027500010628     C                   MOVEL     DDCC          TBEUNI
027600010628     C                   MOVE      WANN          TBEATB
027700010628     C                   UPDATE    TNTBE000
027800010628     C                   ENDIF
027900010628     C     ENDREGISTRA   ENDSR
028000010628      **********************************************************************
028100010628      * RICERCA NUMERO LIBERO
028200010628      **********************************************************************
028300010628     C     SR_RICERCA    BEGSR
028400010628     C*
028500010628     C                   Z-ADD     *ZEROS        WSERIE            4 0
028600010628     C                   MOVEL     'DCC'         TBECOD
028700010628     C     TBECOD        SETLL     TNTBE01L
028800010628     C                   DO        *HIVAL
028900010628     C     TBECOD        READE     TNTBE01L                             31
029000010628     C                   IF        *IN31 = *ON
029100010628     C                   ADD       1             WSERIE
029200010628     C                   MOVEL     WSERIE        V1CSER
029300010628     C                   LEAVE
029400010628     C                   ENDIF
029500010628     C     WSERIE        ADD       1             WSER              4 0
029600010628     C                   MOVEL     TBEKE1        WCODICE           4 0
029700010628     C*Testo che i codici siano successivi e che non esistano buchi
029800010628     C                   IF        WCODICE <> WSER
029900010628     C                   MOVEL     WSER          V1CSER
030000010628     C                   LEAVE
030100010628     C                   ELSE
030200010628     C                   Z-ADD     WCODICE       WSERIE                         SALVO LA SERIE
030300010628     C                   ITER
030400010628     C                   ENDIF
030500010628     C                   ENDDO
030600010628     C                   SETON                                        90
030700010628     C                   ENDSR
030800010628      *****************************************************************
030900010628      * ROUTINE INIZIALE
031000010628      *****************************************************************
031100010628     C     *INZSR        BEGSR
031200010628      *
031300010628     C     *ENTRY        PLIST
031400010628     C                   PARM                    KPJBA
031500010628     C*
031600010628     C                   Z-ADD     1             CODUT
031700010628     C                   CALL      'X§PARUT'
031800010628     C                   PARM                    UT§DSE0F
031900010628     C                   MOVEL     RAGUT         RSUT
032000010628     C                   MOVEL     REC80         CNCR80
032100010628     C*
032200010628      * reperisco data e ora
032300010628     C                   TIME                    W0140
032400010628      * UDATE IN GGMMAAAA
032500010628     C                   MOVE      W0140         WDTGIO
032600010628      * UDATE IN AAAAMMGG
032700010628     C                   Z-ADD     WDTGIO        G02DAT
032800010628     C                   MOVEL     *BLANK        G02ERR
032900010628     C                   CALL      'XSRDA8'
033000010628     C                   PARM                    WLBDAT
033100010628     C                   MOVEL     G02INV        DATEU
033200010628     C                   MOVEL     DATEU         DATOGGI
033300010628     C*
033400010628     C     KTNTBE        KLIST
033500010628     C                   KFLD                    KCOD
033600010628     C                   KFLD                    KKE1
033700010628     C*
033800010628     C                   endsr
033900010628      *---------------------------------------------------------------------------------------------
034000010628** MSG  Lungh. 78                                                            *
034100010628Inserire Numero Serie
034200010628La Serie accetta solo caratteri numerici
034300010628Non è possibile utilizzare questo tasto funzione con la Serie valorizzata
034400010628Inserire il Codice Cliente
034500010628Codice Cliente inesistente
