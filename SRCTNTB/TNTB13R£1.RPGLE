000100010628     H DECEDIT('0,') DATEDIT(*YMD.)
000200010628      ****************************************************************
000300010628      *  RIEPILOGO INDICATORI
000400010628      ****************************************************************
000401010628      * 01 - RECORD ANNULLATO
000402010628      * 02 - *ON VARIAZIONE - *OFF IMMISSIONE
000500010628      * 28 - ERRORE GENERICO DSPF
000600010628      * 31 - GENERICO
000700010628      * 90 - RIEMISSIONE VIDEATA
000800010628      ****************************************************************
001000010628     FAZORG01L  IF   E           K DISK
001100010628     FTNTBE01L  UF A E           K DISK
001200010628     FTNTB13D   CF   E             WORKSTN
001300010628      *------------------------------------------------------------------------*
001400010628     D KCOD            S                   LIKE(TBECOD)
001500010628     D KKE1            S                   LIKE(TBEKE1)
001700010628      *   S C H I E R E
001800010628     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG VIDEO
001900010628     D DATOGGI         S              8  0
002000010628
002100010628     D W0140           S             14  0
002200010628     D WDTGIO          S              8  0
002300010628     D DATEU           S              8  0
002400010628     D*
002700010628      *   D S   I N T E R N E / E S T E R N E
002800010628     D WLBDAT          DS                  INZ
002900010628     D  G02DAT                 1      8  0
003000010628     D  G02INV                 9     16  0
003100010628     D  G02ERR                17     17
003200010628     D  G02TGI                18     22  0
003300010628     D*
003301010628     D TIBS69DS      E DS
003302010628     D DS_CNACO      E DS                  EXTNAME(CNACO00F)
003303010628     D DS_CNIND      E DS                  EXTNAME(CNIND00F)
003304010628     D DS_CNCLP      E DS                  EXTNAME(CNCLP00F)
003305010628     D DS_FNCLS      E DS                  EXTNAME(FNCLS00F)
003400010628     D TIBS02DS      E DS
003500010628     D UT§DSE0F      E DS
003600010628     D CNCR80        E DS
003700010628     D DDCC          E DS
003800010628     D*
003900010628     D KPJBA         E DS
004200010628     D*
004300010628     D*------------------------------------------------------------------------*
004400010628     C                   DO        *HIVAL
004500010628     C* Pulizia 1° Videata
004600010628     C                   IF        (*IN28 = *OFF AND *IN90 = *OFF)
004700010628     C                   MOVE      *BLANKS       V1CSER
004900010628     C                   MOVE      '?'           V1CSER
005000010628     C                   ENDIF
005100010628     C*
005200010628      * Emissione 1° Videata
005300010628     C                   EXFMT     TNTB131
005400010628      * F3=Fine
005500010628     C   KC              LEAVE
005600010628     C*
005700010628      * Controlli 1° videata
005800010628     C                   SETOFF                                       28
005900010628     C                   EXSR      SR_CONTR01
006000010628     C*
006100010628     C   28              ITER
006200010628     C   90              ITER
006201010628     C*
006202010628     C* F13=Ricerca numero libero
006203010628     C   KM              EXSR      SR_RICERCA
006204010628     C   KM              ITER
006400010628      * Caricamento e pulizia seconda videata
006500010628     C                   EXSR      SR_CARVID
006600010628     C*
006700010628      * Emissione 2° Videata
006800010628     C     RIEMETTO      TAG
006900010628     C                   EXFMT     TNTB132
007000010628     C*
007100010628     C                   EVAL      *IN28 = *OFF
007200010628     C*
007300010628      * F3=Fine
007400010628     C   kc              LEAVE
007500010628     C*
007600010628      * F12=Ritorno
007700010628     C   kl              ITER
007800010628     C*
007900010628     C                   EXSR      SR_CONTR02
008000010628     C   28              GOTO      RIEMETTO
008100010628     C*
008101010628     C* F5=Ripristino
008102010628     C   KE              MOVEL     *BLANKS       WANN              1
008103010628     C*
008104010628     C* F16=Annullamento
008105010628     C   KQ              MOVEL     'A'           WANN              1
008106010628     C*
008200010628      * F6=Aggiornamento o F5=Ripristino o F16=Annullamento
008201010628     C                   IF        *INKE = *ON OR *INKF = *ON OR *INKQ = *ON
008300010628     C                   EXSR      SR_REGISTRA
008400010628     C                   ITER
008401010628     C                   ENDIF
008500010628     C*
008600010628     C                   GOTO      RIEMETTO
008700010628     C*
008800010628     C                   ENDDO
008900010628     C*
009000010628     C                   EVAL      *INLR = *ON
009100010628      **********************************************************************
009200010628      * CONTROLLI VIDEO 1
009300010628      **********************************************************************
009400010628     C     SR_CONTR01    BEGSR
009500010628      * Reset indicatori
009600010628     C                   SETOFF                                       90
009800010628     C     '?'           SCAN      V1CSER                                 31
009900010628     C     *IN31         IFEQ      *ON
009901010628     C*Se non ho premuto F13 controllo il ? e il campo valorizzato
009902010628     C                   IF        *INKM = *ON
009903010628     C                   GOTO      TAGF13
009904010628     C                   ENDIF
009905010628     C*
010000010628     C                   EVAL      *IN90 = *ON
010100010628     C                   CLEAR                   TIBS02DS
010200010628     C                   MOVEL     'R'           T02MOD
010300010628     C                   MOVEL     KNSIF         T02SIF
010400010628     C                   MOVEL     'DCC'         T02COD
010500010628     C                   CALL      'TIBS02R'
010600010628     C                   PARM                    KPJBA
010700010628     C                   PARM                    TIBS02DS
010800010628     C                   IF        T02ERR =  *BLANKS
010900010628     C                   MOVEL     T02KE1        V1CSER
011000010628     C                   SETON                                        90
011100010628     C                   ELSE
011200010628     C                   SETON                                        28
011300010628     C                   MOVEL     MSG(1)        V1CMSG
011400010628     C                   GOTO      ENDSRCONTR
011500010628     C                   ENDIF
011600010628     C                   ENDIF
011700010628
011800010628      * controllo se campo valorizzato
011900010628     C                   IF        V1CSER = *ZEROS OR V1CSER = *BLANKS
011901010628     C                   IF        *INKM = *OFF
012100010628     C                   SETON                                        28
012200010628     C                   MOVEL     MSG(1)        V1CMSG
012300010628     C                   GOTO      ENDSRCONTR
012400010628     C                   ENDIF
012401010628     C                   ENDIF
012402010628     C*
012500010628     C* controllo che contenga solo caratteri numerici
012502010628     C                   IF        %SUBST(V1CSER:1:1) < '0' OR
012503010628     C                             %SUBST(V1CSER:2:1) < '0' OR
012504010628     C                             %SUBST(V1CSER:3:1) < '0' OR
012505010628     C                             %SUBST(V1CSER:4:1) < '0'
012507010628     C                   SETON                                        28        controllo P.O.emi.
012508010628     C                   MOVEL     MSG(2)        V1CMSG
012509010628     C                   GOTO      ENDSRCONTR                                   normale
012510010628     C                   ENDIF
012511010628     C*
012512010628     C     TAGF13        TAG
012513010628     C*
012514010628     C* Se premuto F13 il campo non deve essere valorizzato (escludo '?')
012515010628     C                   IF        *INKM = *ON AND *IN31 = *OFF AND
012516010628     C                             V1CSER <> *BLANKS AND V1CSER <> *ZEROS
012517010628     C                   SETON                                        28
012518010628     C                   MOVEL     MSG(3)        V1CMSG
012519010628     C                   GOTO      ENDSRCONTR
012520010628     C                   ENDIF
012522010628     C*
014400010628     C     ENDSRCONTR    endsr
014500010628      **********************************************************************
014600010628      * CONTROLLI VIDEO 2
014700010628      **********************************************************************
014800010628     C     Sr_Contr02    begsr
014900010628      * Reset indicatori
015000010628     C                   SETOFF                                       28
015100010628     C*
015200010628     C                   IF        V2CKSC = *ZEROS
015500010628     C                   SETON                                        28
015600010628     C                   MOVEL     MSG(4)        V2CMSG
015700010628     C                   GOTO      ENDSRCONT2
016700010628     C                   ENDIF
016800010628     C*
016801010628     C                   CLEAR                   TIBS69DS
016802010628     C                   Z-ADD     V2CKSC        I69KAC
016803010628     C                   CALL      'TIBS69R'
016804010628     C                   PARM                    TIBS69DS
016805010628     C                   PARM                    DS_CNACO
016806010628     C                   PARM                    DS_CNIND
016807010628     C                   PARM                    DS_CNCLP
016808010628     C                   PARM                    DS_FNCLS
016809010628     C*
016810010628     C                   IF        O69ERR = *BLANKS
016811010628     C                   MOVEL     ACORAG        V2DKSC
016812010628     C                   ELSE
016813010628     C                   SETON                                        28
016814010628     C                   MOVEL     MSG(5)        V2CMSG
016815010628     C                   GOTO      ENDSRCONT2
016816010628     C                   ENDIF
016817010628     C*
016900010628     C     ENDSRCONT2    ENDSR
017000010628      **********************************************************************
017100010628      * CARICAMENTO SECONDA VIDEATA
017200010628      **********************************************************************
017300010628     C     SR_CARVID     BEGSR
017400010628     C*
017401010628     C                   SETOFF                                       01        ANNULLAMENTO
017500010628     C                   MOVEL     V1CSER        V2CSER
017501010628     C                   MOVEL     *ZEROS        V2CKSC
017502010628     C                   MOVEL     *BLANKS       V2DKSC
017503010628      *
017900010628     C                   MOVEL     V1CSER        KKE1
018000010628     C                   MOVEL     'DCC'         KCOD
018100010628     C     KTNTBE        CHAIN     TNTBE01L                           31
018200010628     C                   IF        *IN31 = *OFF
018201010628     C                   SETON                                        02        VARIAZIONE
018300010628     C                   MOVEL     TBEUNI        DDCC
018400010628     C                   MOVEL     §DCCKSC       V2CKSC
018700010628     C                   ENDIF
018701010628     C*
018702010628     C                   CLEAR                   TIBS69DS
018703010628     C                   Z-ADD     V2CKSC        I69KAC
018704010628     C                   CALL      'TIBS69R'
018705010628     C                   PARM                    TIBS69DS
018706010628     C                   PARM                    DS_CNACO
018707010628     C                   PARM                    DS_CNIND
018708010628     C                   PARM                    DS_CNCLP
018709010628     C                   PARM                    DS_FNCLS
018710010628     C*
018711010628     C                   IF        O69ERR = *BLANKS
018712010628     C                   MOVEL     ACORAG        V2DKSC
018717010628     C                   ENDIF
018718010628     C*
018719010628     C                   MOVEL     *BLANKS       V2VAIM                         VARIAZ./IMMISSIONE
018720010628     C*
018721010628     C                   IF        *IN02 = *ON                                  VARIAZIONE
018722010628     C                   IF        TBEATB = *BLANKS
018723010628     C                   MOVEL     'VARIAZIONE'  V2VAIM
018724010628     C                   ELSE
018725010628     C                   MOVEL     'ANNULLATO '  V2VAIM
018726010628     C                   SETON                                        01        ANNULLATO
018727010628     C                   ENDIF
018728010628     C                   ELSE                                                   IMMISSIONE
018729010628     C                   MOVEL     'IMMISSIONE'  V2VAIM
018730010628     C                   ENDIF
018731010628     C*
018900010628     C                   ENDSR
019000010628      **********************************************************************
019100010628      * REGISTRAZIONE DATI IMMESSI
019200010628      **********************************************************************
019300010628     C     SR_REGISTRA   BEGSR
019400010628
019500010628     C                   MOVEL     'DCC'         KCOD
019600010628     C                   MOVEL     V2CSER        KKE1
019700010628     C     KTNTBE        CHAIN     TNTBE01L                           31
019800010628     C                   IF        *IN31 = *ON
019801010628     C                   MOVE      *BLANKS       TBEATB
019900010628     C                   MOVEL     'DCC'         TBECOD
020000010628     C                   MOVEL     V2CSER        TBEKE1
020200010628     C                   MOVE      'S'           TBEFTT
020300010628     C                   MOVE      *BLANKS       TBEFTR
020500010628     C                   MOVEL     V2CKSC        §DCCKSC
020800010628     C                   MOVEL     DDCC          TBEUNI
020900010628     C                   WRITE     TNTBE000
021000010628     C                   ELSE
021200010628     C                   MOVE      'S'           TBEFTT
021300010628     C                   MOVE      *BLANKS       TBEFTR
021500010628     C                   MOVEL     V2CKSC        §DCCKSC
021800010628     C                   MOVEL     DDCC          TBEUNI
021801010628     C                   MOVE      WANN          TBEATB
021900010628     C                   UPDATE    TNTBE000
022000010628     C                   ENDIF
022100010628     C     ENDREGISTRA   ENDSR
022101010628      **********************************************************************
022102010628      * RICERCA NUMERO LIBERO
022103010628      **********************************************************************
022104010628     C     SR_RICERCA    BEGSR
022105010628     C*
022106010628     C                   Z-ADD     *ZEROS        WSERIE            4 0
022107010628     C                   MOVEL     'DCC'         TBECOD
022108010628     C     TBECOD        SETLL     TNTBE01L
022109010628     C                   DO        *HIVAL
022110010628     C     TBECOD        READE     TNTBE01L                             31
022111010628     C                   IF        *IN31 = *ON
022112010628     C                   ADD       1             WSERIE
022113010628     C                   MOVEL     WSERIE        V1CSER
022114010628     C                   LEAVE
022115010628     C                   ENDIF
022116010628     C     WSERIE        ADD       1             WSER              4 0
022117010628     C                   MOVEL     TBEKE1        WCODICE           4 0
022118010628     C*Testo che i codici siano successivi e che non esistano buchi
022119010628     C                   IF        WCODICE <> WSER
022120010628     C                   MOVEL     WSER          V1CSER
022121010628     C                   LEAVE
022122010628     C                   ELSE
022123010628     C                   Z-ADD     WCODICE       WSERIE                         SALVO LA SERIE
022124010628     C                   ITER
022125010628     C                   ENDIF
022126010628     C                   ENDDO
022127010628     C                   SETON                                        90
022131010628     C                   ENDSR
022200010628      *****************************************************************
022300010628      * ROUTINE INIZIALE
022400010628      *****************************************************************
022500010628     C     *INZSR        BEGSR
022600010628      *
022700010628     C     *ENTRY        PLIST
022800010628     C                   PARM                    KPJBA
022900010628     C*
023000010628     C                   Z-ADD     1             CODUT
023100010628     C                   CALL      'X§PARUT'
023200010628     C                   PARM                    UT§DSE0F
023300010628     C                   MOVEL     RAGUT         RSUT
023400010628     C                   MOVEL     REC80         CNCR80
023500010628     C*
023600010628      * reperisco data e ora
023700010628     C                   TIME                    W0140
023800010628      * UDATE IN GGMMAAAA
023900010628     C                   MOVE      W0140         WDTGIO
024000010628      * UDATE IN AAAAMMGG
024100010628     C                   Z-ADD     WDTGIO        G02DAT
024200010628     C                   MOVEL     *BLANK        G02ERR
024300010628     C                   CALL      'XSRDA8'
024400010628     C                   PARM                    WLBDAT
024500010628     C                   MOVEL     G02INV        DATEU
024600010628     C                   MOVEL     DATEU         DATOGGI
024700010628     C*
024800010628     C     KTNTBE        KLIST
024900010628     C                   KFLD                    KCOD
025000010628     C                   KFLD                    KKE1
025100010628     C*
029000010628     C                   endsr
029100010628      *---------------------------------------------------------------------------------------------
029200010628** MSG  Lungh. 78                                                            *
029300010628Inserire Numero Serie
029301010628La Serie accetta solo caratteri numerici
029400010628Non è possibile utilizzare questo tasto funzione con la Serie valorizzata
029500010628Inserire il Codice Cliente
029600010628Codice Cliente inesistente
