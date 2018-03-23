000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200951113     H* TNTL35R  *----------------------------------------------------*
000300951110     H*----------*                                                    *
000400951110     H* CALCOLO DATA TEORICA PARTENZA/ARRIVO F.VIAGGIO E CONTROLLO    **
000500951110     H* ESISTENZA DI ANTICIPO/RITARDO NELLA PARTENZA/ARRIVO F.VIAGGIO *
000600951110     H*---------------------------------------------------------------*
000700951110     FTNTLO01L  IF   E           K DISK
000800960202     FTNTLR03L  IF   E           K DISK
000900960826     FTNTLR01L  IF   E           K DISK
001000960826     F                                     RENAME(TNTLR000:TNTLR1)
001100951110     FTABEL00F  IF   E           K DISK
001200960117     FAZCLN01L  IF   E           K DISK
001300951110     D*
001400951110     D* DEFINIZIONE SCHIERE
001500951110     D GFV             S              1    DIM(20) CTDATA PERRCD(20)            GG PARTENZA
001600951110     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MESSAGGI ERRORE
001700960117     D S1Y             S              1    DIM(10)                              SIMBOLI CALEND
001800960118     D FET             S              1    DIM(10)                              FESTIVO TRAINI
001900951110     D*
002000951110     D WLBDAT          DS
002100951110     D  G02DAT                 1      8  0
002200951110     D  G02INV                 9     16  0
002300951110     D  G02ERR                17     17
002400951110     D  G02TGI                18     22  0
002500951110     D WGIDAT          DS
002600951110     D  GI8DAT                 1      8  0
002700951110     D  GI8INV                 9     16  0
002800951110     D  GI8TGI                17     21  0
002900951110     D WXDTOR          DS
003000951110     D  WXDT1                  1      8  0
003100951110     D  WXHM1                  9     12  0
003200951110     D  WXDT2                 13     20  0
003300951110     D  WXHM2                 21     24  0
003400951110     D  WXFLG                 25     25
003500951110     D  WXMIN                 26     30  0
003600951110     D*
003700951110     D                 DS
003800951110     D  DATA1                  1      8  0
003900951110     D  ORA1                   9     12  0
004000951110     D  COMDO1                 1     12  0
004100951110     D                 DS
004200951110     D  DATA2                  1      8  0
004300951110     D  ORA2                   9     12  0
004400951110     D  COMDO2                 1     12  0
004500951113     D                 DS
004600951113     D  COMDRE                 1      8  0
004700951113     D  COMHRE                 9     12  0
004800951113     D  COMDHR                 1     12  0
004900960117     D                 DS
005000960117     D  CLNPOM                 1     31
005100960117     D  POM                    1     31
005200960117     D                                     DIM(31)                              FESTIV.POMER.
005300951110     D*
005400951113     D* DS PER TNTL35R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
005500951113     D DSTL35        E DS                  EXTNAME(TNTL35DS)
005600040622     D* DS PER TIBS02R - GESTIONE TNTBE00F
005700040622     D DSBS02        E DS                  EXTNAME(TIBS02DS)
005800951113     D*
005900951110     D DS3I          E DS
006000951113     D DS7A2         E DS
006100960117     D DS1Y          E DS
006200040622     D*** DS1X          E DS
006300951110     D KPJBA         E DS
006400951110     I/SPACE 3
006500951110     C*****************************************************************
006600951110     C* RIEPILOGO INDICATORI
006700951110     C*****************************************************************
006800960122     C* 30/32 - DI COMODO
006900951110     C*****************************************************************
007000951110     C     *ENTRY        PLIST
007100951110     C                   PARM                    KPJBA
007200951113     C                   MOVEL     KPJBU         DSTL35
007300960216     C                   CLEAR                   D35MIN
007400951113     C*
007500951110     C*---------------------------------------------------------------*
007600951113     C* TIPO LANCIO: D35TLA = "C" -           CHIUDO CON LR
007700951113     C*              D35TLA = "L" - ELABORO E CHIUDO CON LR
007800951113     C*              D35TLA = " " - ELABORO E CHIUDO IN  RETRN
007900951113    0C     D35TLA        IFNE      'C'
008000951113     C*
008100951110     C******
008200951110     C** CALCOLO LA DATA TEORICA PARTENZA/ARRIVO DEL FOGLIO VIAGGIO
008300951110     C******
008400951113    1C     D35TUP        IFEQ      'D'
008500951113     C     D35TUP        OREQ      'E'
008600951110     C*
008700960122     C* SE IL NUMERO FERMATA PASSATO E' VUOTO OCCORRE CHAINARE IL
008800960122     C*   FILE DELLE FERMATE PER REPERIRLO
008900960122   2AC     D35NFM        IFEQ      0
009000960202     C     KTLR          CHAIN     TNTLR03L                           32
009100960202     C     *IN32         DOWEQ     *OFF
009200960202     C*
009300960202     C     D35EPA        IFEQ      'P'
009400960202     C     TLRCAR        ANDEQ     'S'
009500960202     C*
009600960202     C     D35EPA        OREQ      'A'
009700960202     C     TLRSCA        ANDEQ     'S'
009800960122     C                   MOVEL     TLRNFM        WNFM
009900960202     C                   SETON                                        32
010000960202     C*
010100960202     C                   ELSE
010200960202     C     KTLR          READE     TNTLR03L                               32
010300960122     C                   ENDIF
010400960202     C                   ENDDO
010500960122     C*
010600960122  X2AC                   ELSE
010700960122     C                   MOVEL     D35NFM        WNFM
010800960122   2AC                   ENDIF
010900960122     C*
011000960122     C* PROSEGUO SOLO SE  NUMERO FERMATA > 0
011100960122   2AC     WNFM          IFGT      0
011200960122     C*
011300951107     C* REPERISCO IL GIORNO DELLA SETTIMANA RELATIVO ALLA DATA TRAINO
011400960219     C* GIRO LA DATA
011500960219     C                   CLEAR                   WLBDAT
011600960219     C                   MOVE      D35DTN        G02INV
011700960219     C                   MOVE      '3'           G02ERR
011800960219     C                   CALL      'XSRDA8'
011900960219     C                   PARM                    WLBDAT
012000960219     C                   MOVE      G02DAT        W0020             2 0
012100960219     C                   MOVEL     G02DAT        GGMMAA
012200951110     C                   MOVE      W0020         GGMMAA
012300951110     C*
012400951107     C                   CALL      'XGIOSE1'
012500951110     C                   PARM                    GGMMAA            6
012600951110     C                   PARM                    GIOSET            1
012700951107     C*
012800951110     C* CHAINO FILE ORARI PER CALCOLARE DATA TEORICA PARTENZA/ARRIVO
012900951106     C*   RISPETTO ALLA DATA TRAINO
013000951110     C     KTLO          CHAIN     TNTLO01L                           30
013100951110     C*
013200960122     C* SE NON TROVATO O ANNULLATO, PROVO CON IL GIORNO DELLA
013300960122     C*   SETTIMANA = *BLANKS
013400960122    2C     *IN30         IFEQ      *ON
013500960122     C     TLOATB        ORNE      ' '
013600960117     C                   MOVEL     ' '           GIOSET
013700960117     C     KTLO          CHAIN     TNTLO01L                           30
013800960122    2C                   ENDIF
013900951110     C*
014000951110     C* SE NON TROVATO, PASSO AL PGM CHIAMANTE UN MESSAGGIO DI ERRORE
014100951110    2C     *IN30         IFEQ      *ON
014200960122     C     TLOATB        ORNE      ' '
014300951113     C                   MOVEL     'E'           D35ERR
014400951113     C                   MOVEL     MSG(1)        D35MSG
014500951110     C                   GOTO      FINE
014600951110    2C                   ENDIF
014700951110     C*
014800951110     C* 30 OFF - TROVATO RECORD
014900951110    2C     *IN30         IFEQ      *OFF
015000951113     C*
015100951113    3C     D35EPA        IFEQ      'P'
015200951113     C                   MOVEL     TLOGGP        WGGPA
015300951113   X3C                   ELSE
015400951113     C                   MOVEL     TLOGGA        WGGPA
015500951113    3C                   ENDIF
015600951106     C*
015700951102     C* GIORNO=A: LA DATA PARTENZA TEORICA CORRISPONDE ALLA DATA TRAINO
015800951113    3C     WGGPA         IFEQ      'A'
015900951113     C                   Z-ADD     D35DTN        D35DTE
016000951110   X3C                   ELSE
016100960118     C* GIORNO<>A: AGGIUNGO ALLA DATA TRAINO IL GIORNO DI PARTENZA
016200951103     C                   CLEAR                   WLBDAT
016300951113     C                   MOVE      D35DTN        G02INV
016400951103     C                   MOVE      '3'           G02ERR
016500951103     C                   CALL      'XSRDA8'
016600951103     C                   PARM                    WLBDAT
016700951103     C*
016800951103     C                   Z-ADD     1             C                 3 0
016900951113     C     WGGPA         LOOKUP    GFV(C)                                 31
017000951103     C     G02TGI        ADD       C             GI8TGI
017100951103     C                   CALL      'XSRGI8'
017200951103     C                   PARM                    WGIDAT
017300951113     C                   Z-ADD     GI8INV        D35DTE
017400960118     C* GIORNO>B: VERIFICO PRESENZA DI FESTIVI CON BLOCCO TRAFFICO
017500960118     C*           FRA DATA TRAINO E DATA TEORICA CALCOLATA: IN TAL CASO
017600960118     C*           SLITTO DATA
017700960117     C     WGGPA         IFGT      'B'
017800960117     C                   EXSR      CALDAT
017900960117     C                   END
018000951110    3C                   ENDIF
018100951113     C*
018200951113    3C     D35EPA        IFEQ      'P'
018300951113     C                   MOVEL     TLOHHP        D35HTE
018400951113   X3C                   ELSE
018500951113     C                   MOVEL     TLOHHA        D35HTE
018600951113    3C                   ENDIF
018700960826     C* SE CALCOLATA DATA ARRIVO FESTIVA VERIFICO SE SLITTARE DATA
018800960826     C* IN BASE ALLA SOGLIA
018900960826    3C     D35EPA        IFEQ      'A'
019000960826     C                   MOVEL     D35DTE        KANN
019100960826     C                   MOVE      D35DTE        W0040             4 0
019200960826     C                   MOVEL     W0040         KMES
019300960826     C                   MOVE      W0040         GG                2 0
019400960826     C                   EXSR      CTRFES
019500960826    4C     WFES          IFNE      *BLANKS
019600960826     C* Se fil.di arrivo=0 la prendo chainando file delle fermate
019700960826    5C     D35FIL        IFEQ      *ZEROS
019800960826     C     KTLR1         CHAIN     TNTLR01L                           32
019900960826     C  N32              MOVE      TLRFIL        W0030             3 0
020000960826     C   32              CLEAR                   W0030
020100960826   X5C                   ELSE
020200960826     C                   MOVE      D35FIL        W0030             3 0
020300960826    5C                   END
020400960826     C* Reperisco ora di soglia e se necessario ricalcolo data teorica
020500960826     C                   EXSR      REPSOG
020600960826    4C                   ENDIF
020700960826    3C                   ENDIF
020800960826     C*
020900951110    2C                   ENDIF
021000951110     C*
021100960122   2AC                   ENDIF
021200951110    1C                   ENDIF
021300951110     C*
021400951110     C******
021500951110     C** CONTROLLO SE ESISTE ANTICIPO/RITARDO NELLA PARTENZA/ARRIVO
021600951110     C**   DEL FOGLIO VIAGGIO
021700951110     C******
021800951113    1C     D35TUP        IFEQ      'C'
021900951113     C     D35TUP        OREQ      'E'
022000960220     C                   CLEAR                   D35FLG
022100960220     C                   CLEAR                   D35MIN
022200960220     C*
022300960220     C* SE SONO UGUALI SICURAMENTE NON C'E' RITARDO /ANTICIPO
022400960220   1AC     D35DRE        IFNE      D35DTE
022500960220     C     D35HRE        ORNE      D35HTE
022600951110     C*
022700951110     C* SOTTRAGGO I MINUTI DI TOLLERANZA ALLA DATA TEORICA
022800951103     C                   CLEAR                   WXDTOR
022900951113     C                   MOVEL     D35DTE        WXDT1
023000951113     C                   MOVEL     D35HTE        WXHM1
023100951103     C                   MOVEL     '-'           WXFLG
023200951113    2C     D35EPA        IFEQ      'P'
023300951103     C                   Z-ADD     §3IMNA        WXMIN
023400951110   X2C                   ELSE
023500951110     C                   Z-ADD     §7AMNA        WXMIN
023600951110    2C                   ENDIF
023700951110     C*
023800951103     C                   CALL      'XDTORA'
023900951103     C                   PARM                    WXDTOR
024000951103     C                   MOVEL     WXDT2         DATA1
024100951103     C                   MOVE      WXHM2         ORA1
024200951103     C*
024300951110     C* AGGIUNGO I MINUTI DI TOLLERANZA ALLA DATA TEORICA
024400951103     C                   CLEAR                   WXDTOR
024500951113     C                   MOVEL     D35DTE        WXDT1
024600951113     C                   MOVEL     D35HTE        WXHM1
024700951103     C                   MOVEL     '+'           WXFLG
024800951113    2C     D35EPA        IFEQ      'P'
024900951103     C                   Z-ADD     §3IMNR        WXMIN
025000951110   X2C                   ELSE
025100951110     C                   Z-ADD     §7AMNR        WXMIN
025200951110    2C                   ENDIF
025300951110     C*
025400951103     C                   CALL      'XDTORA'
025500951103     C                   PARM                    WXDTOR
025600951103     C                   MOVEL     WXDT2         DATA2
025700951103     C                   MOVE      WXHM2         ORA2
025800951103     C*
025900951106     C* CONTROLLO SE DATA/ORA PARTENZA EFFETTIVA E' COMPRESA NEL RANGE
026000951106     C*   DI TEMPO ALL'INTERNO DEL QUALE NON SUSSISTE RITARDO/ANTICIPO
026100951113     C                   MOVEL     D35DRE        COMDRE
026200951113     C                   MOVEL     D35HRE        COMHRE
026300951113     C*
026400960125     C* A N T I C I P O
026500951113    2C     COMDHR        IFLT      COMDO1
026600960125     C                   MOVEL     'A'           D35FLG
026700960125   X2C                   ELSE
026800960125     C* R I T A R D O
026900960216    3C     COMDHR        IFGT      COMDO2
027000951113     C                   MOVEL     'R'           D35FLG
027100960216    3C                   ENDIF
027200951110    2C                   ENDIF
027300960216     C* SE E' PRESENTE DICO QUANTI MINUTI SONO
027400960216     C                   CLEAR                   WXDTOR
027500960216     C                   MOVEL     D35DTE        WXDT1
027600960216     C                   MOVEL     D35HTE        WXHM1
027700960216     C                   MOVEL     D35DRE        WXDT2
027800960216     C                   MOVEL     D35HRE        WXHM2
027900960216     C                   MOVEL     'D'           WXFLG
028000960216     C                   CALL      'XDTORA'
028100960216     C                   PARM                    WXDTOR
028200960216     C                   MOVEL     WXMIN         D35MIN
028300951103     C*
028400960220   1AC                   ENDIF
028500960220    1C                   ENDIF
028600951113    0C                   ENDIF
028700951110     C*
028800951110     C     FINE          TAG
028900951113     C                   MOVEL     DSTL35        KPJBU
029000951113     C*
029100951113     C* CHIUSURA PGM
029200951113    0C     D35TLA        IFEQ      ' '
029300951113     C                   RETURN
029400951113   X0C                   ELSE
029500951113     C                   SETON                                        LR
029600951113    0C                   ENDIF
029700040622     c*
029800040622     c     *inlr         ifeq      *on
029900040622     C                   CLEAR                   DSBS02
030000040622     C                   MOVEL     'C'           T02TLA
030100040622     C                   CALL      'TIBS02R'
030200040622     C                   PARM                    KPJBA
030300040622     C                   PARM                    DSBS02
030400040622     c                   endif
030500960118     C*
030600960117     C*****************  CALCOLO DATA TEORICA  ************************
030700960117     C     CALDAT        BEGSR
030800960117     C* QUESTA ROUTINE VIENE RICHIAMATA SE GIORNO TEORICO>'B':
030900960117     C* SE GIORNO TEORICO = 'X' OCCORRE DETERMINARE LA DATA TENENDO
031000960117     C* CONTO DI EVENTUALI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LA
031100960117     C* DATA TRAINO E 'X' E IN TAL CASO SLITTARE LA DATA TEORICA DI
031200960117     C* TANTI GIORNI QUANTI SONO I FESTIVI CALCOLATI
031300960117     C                   MOVE      D35DTE        WDTAA                          DATA A
031400960117     C                   MOVE      *ZEROS        WDAT
031500960117     C                   MOVE      *ZEROS        Y                 2 0
031600960117     C                   Z-ADD     1             X                 2 0
031700960117     C* CALCOLO DATA TRAINO+1
031800960117     C                   CLEAR                   WGIDAT
031900960117     C     G02TGI        ADD       1             GI8TGI
032000960117     C                   CALL      'XSRGI8'
032100960117     C                   PARM                    WGIDAT
032200960117     C                   MOVE      GI8INV        WDAT
032300960117     C*
032400960117     C     CICLO         TAG
032500960117     C*
032600960117    1C     WDAT          DOWLT     WDTAA
032700960117     C* VERIFICO SE DATA FESTIVA
032800960117     C                   MOVEL     WDAT          KANN
032900960117     C                   MOVE      WDAT          W0040             4 0
033000960117     C                   MOVEL     W0040         KMES
033100960117     C                   MOVE      W0040         GG                2 0
033200960117     C                   EXSR      CTRFES
033300960117    2C     WFES          IFEQ      'B'
033400960117     C                   ADD       1             Y                              fest. blocco
033500960117    2C                   END
033600960117     C* CALCOLO DATA TRAINO+X
033700960117     C                   ADD       1             X
033800960117     C                   CLEAR                   WGIDAT
033900960117     C     G02TGI        ADD       X             GI8TGI
034000960117     C                   MOVE      GI8TGI        COMTGI
034100960117     C                   CALL      'XSRGI8'
034200960117     C                   PARM                    WGIDAT
034300960117     C                   MOVE      GI8INV        WDAT
034400960117    1C                   ENDDO
034500960117     C* TROVATI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LE DUE DATE:
034600960117    1C     Y             IFGT      0
034700960118     C* se anche data teorica festiva slitto di un giorno in più
034800960118     C                   MOVEL     D35DTE        KANN
034900960118     C                   MOVE      D35DTE        W0040             4 0
035000960118     C                   MOVEL     W0040         KMES
035100960118     C                   MOVE      W0040         GG                2 0
035200960118     C                   EXSR      CTRFES
035300960118    2C     WFES          IFEQ      'B'
035400960118     C                   ADD       1             Y
035500960118     C                   END
035600960117     C* CALCOLO DATA FVT NUOVA AGGIUNGENDO IL NUMERO DI FESTIVI
035700960117     C                   CLEAR                   WGIDAT
035800960117     C     COMTGI        ADD       Y             GI8TGI
035900960117     C                   CALL      'XSRGI8'
036000960117     C                   PARM                    WGIDAT
036100960117     C*          RIPETO IL CONTROLLO PRECEDENTE CON
036200960117     C*          DATA DAL = DATA FVT VECCHIO E DATA AL = DATA FVT NUOVO
036300960117     C*          (IL CONTROLLO NON OCCORRE RIPETERLO SE HO SLITTATO DI
036400960117     C*          UN SOLO GIORNO)
036500960117    2C     Y             IFEQ      1
036600960117     C                   MOVE      GI8INV        D35DTE
036700960117   X2C                   ELSE
036800960117     C                   MOVE      GI8INV        WDTAA
036900960117     C*
037000960117     C                   MOVE      *ZEROS        Y                 2 0
037100960117     C                   ADD       1             X                 2 0
037200960117     C                   CLEAR                   WGIDAT
037300960117     C     G02TGI        ADD       X             GI8TGI
037400960117     C                   CALL      'XSRGI8'
037500960117     C                   PARM                    WGIDAT
037600960117     C                   MOVE      GI8INV        WDAT
037700960117     C                   GOTO      CICLO
037800960117    2C                   END
037900960117   X1C                   ELSE
038000960117     C* NON TROVATI GIORNI FESTIVI CON BLOCCO TRAFFICO FRA LE DUE DATE
038100960117     C                   MOVE      WDTAA         D35DTE
038200960117    1C                   END
038300960117     C                   ENDSR
038400960117     C*************  CONTROLLO SE GIORNO FESTIVO  *********************
038500960117     C     CTRFES        BEGSR
038600960117     C                   CLEAR                   WFES
038700960117     C     KCLN          CHAIN     AZCLN01L                           30
038800960117    1C     *IN30         IFEQ      *OFF
038900960117     C                   Z-ADD     1             C
039000960117     C     POM(GG)       LOOKUP    S1Y(C)                                 30
039100960117     C   30              MOVE      FET(C)        WFES              1
039200960117    1C                   END
039300960117     C                   ENDSR
039400960826     C*********  REPERIMENTO ORA DI SOGLIA PER ARRIVO TRAZIONI  *******
039500960826     C     REPSOG        BEGSR
039600040622     C***                MOVEL     '1X'          COD
039700040622     C***                MOVEL     W0030         W006A             6
039800040622     C***                MOVE      'OSA'         W006A
039900040622     C***                MOVEL(P)  W006A         KEY
040000040622     C***                MOVE      *ZEROS        II                1 0
040100040622    1C***  II            DOWLE     1
040200040622     C***  KTAB          CHAIN     TABEL                              32
040300040622    2C***  *IN32         IFEQ      *OFF
040400040622     C***  TBLFLG        ANDEQ     *BLANKS
040500960826     C* Trovata soglia:ricalcolo data teor.(se ora teorica>ora soglia)
040600960826     C*                ed esco dal ciclo
040700040622     C***                Z-ADD     9             II
040800040622     C***                MOVEL     TBLUNI        DS1X
040900040622      * Aggancio tabella SOT per ora soglia arrivo trazioni
041000040622     C                   CLEAR                   DSBS02
041100040622     C                   MOVEL     'C'           T02MOD
041200040622     C                   MOVEL     KNSIF         T02SIF
041300040622     C                   MOVEL     'SOT'         T02COD
041400040622     C                   MOVEL     w0030         T02KE1
041500040622     C                   CALL      'TIBS02R'
041600040622     C                   PARM                    KPJBA
041700040622     C                   PARM                    DSBS02
041800040622     C* Non trovata soglia del ter-arr.: cerco soglia generica
041900040622     C     T02ERR        IFne      *BLANKS
042000040622     C                   CLEAR                   DSBS02
042100040622     C                   MOVEL     'C'           T02MOD
042200040622     C                   MOVEL     KNSIF         T02SIF
042300040622     C                   MOVEL     'SOT'         T02COD
042400040622     C                   MOVEL     'GEN'         T02KE1
042500040622     C                   CALL      'TIBS02R'
042600040622     C                   PARM                    KPJBA
042700040622     C                   PARM                    DSBS02
042800040622     c                   ENDIF
042900040622     c                   movel     t02uni        wsotho            4 0
043000040622    3C     D35HTE        IFGE      wsotho
043100960826     C                   CLEAR                   WLBDAT
043200960826     C                   MOVE      D35DTE        G02INV
043300960826     C                   MOVE      '3'           G02ERR
043400960826     C                   CALL      'XSRDA8'
043500960826     C                   PARM                    WLBDAT
043600960826     C                   MOVE      *ZEROS        X
043700960826     C* Incremento di 1 giorno la data teorica finchè la data risultant
043800960826     C* non è festiva
043900960826    4C     WFES          DOUEQ     *BLANKS
044000960826     C                   ADD       1             X
044100960826     C                   CLEAR                   WGIDAT
044200960826     C     G02TGI        ADD       X             GI8TGI
044300960826     C                   CALL      'XSRGI8'
044400960826     C                   PARM                    WGIDAT
044500960826     C* Verifico se data festiva
044600960826     C                   MOVEL     GI8INV        KANN
044700960826     C                   MOVE      GI8INV        W0040
044800960826     C                   MOVEL     W0040         KMES
044900960826     C                   MOVE      W0040         GG
045000960826     C                   EXSR      CTRFES
045100960826     C     WFES          IFEQ      *BLANKS
045200960826     C                   Z-ADD     GI8INV        D35DTE
045300960826     C                   ENDIF
045400960826    4C                   ENDDO
045500960826    3C                   ENDIF
045600040622   X2C***                ELSE
045700960826     C* Non trovata soglia della fil. di arrivo: cerco soglia generica
045800040622     C***                ADD       1             II
045900040622     C***                MOVEL     '   '         KEY
046000040622    2C***                ENDIF
046100040622    1C***                ENDDO
046200960826     C                   ENDSR
046300960117     C*
046400951110     C*--- SR INIZIALE -----------------------------------------------*
046500951110     C     *INZSR        BEGSR
046600951110     C*
046700951110     C****  KLIST  ****
046800951110     C* ACCESSO TABEL00F
046900951110     C     KTAB          KLIST
047000951110     C                   KFLD                    CODUT
047100951110     C                   KFLD                    COD
047200951110     C                   KFLD                    KEY
047300960118     C     KTAB0         KLIST
047400960118     C                   KFLD                    CODUT
047500960118     C                   KFLD                    COD
047600951110     C* ACCESSO TNTLO01L
047700951113     C     KTLO          KLIST
047800951113     C                   KFLD                    D35TRN                                     FOGLIO
047900951113     C                   KFLD                    D35DDE
048000960122     C                   KFLD                    WNFM
048100951110     C                   KFLD                    GIOSET                                     NE
048200960826     C* ACCESSO TNTLR03L
048300960122     C     KTLR          KLIST
048400960122     C                   KFLD                    D35TRN                                     FOGLIO
048500960202     C                   KFLD                    D35DDE                                     FOGLIO
048600960122     C                   KFLD                    D35TFM
048700960122     C                   KFLD                    D35FIL
048800960826     C* ACCESSO TNTLR01L
048900960826     C     KTLR1         KLIST
049000960826     C                   KFLD                    D35TRN                                     FOGLIO
049100960826     C                   KFLD                    D35DDE                                     FOGLIO
049200960826     C                   KFLD                    D35NFM                                     FOGLIO
049300960117     C* ACCESSO A CALENDARIO BARTOLINI
049400960117     C     KCLN          KLIST
049500960117     C                   KFLD                    KTFP
049600960117     C                   KFLD                    KTFA
049700960117     C                   KFLD                    KANN
049800960117     C                   KFLD                    KMES
049900951110     C*
050000951110     C****  DEFINIZIONE CAMPI  ****
050100951110     C     *LIKE         DEFINE    TBLKUT        CODUT
050200951110     C     *LIKE         DEFINE    TBLCOD        COD
050300951110     C     *LIKE         DEFINE    TBLKEY        KEY
050400951113     C     *LIKE         DEFINE    TLOGGP        WGGPA
050500960122     C     *LIKE         DEFINE    TLONFM        WNFM
050600960117     C     *LIKE         DEFINE    CLNANN        KANN
050700960117     C     *LIKE         DEFINE    CLNMES        KMES
050800960117     C     *LIKE         DEFINE    CLNTFP        KTFP
050900960117     C     *LIKE         DEFINE    CLNTFA        KTFA
051000960117     C     *LIKE         DEFINE    GI8TGI        COMTGI
051100960117     C     *LIKE         DEFINE    D35DTN        WDTAA
051200960117     C     *LIKE         DEFINE    D35DTN        WDAT
051300951110     C*
051400960201    1C     D35TLA        IFNE      'C'
051500951110     C****  IMPOSTO CAMPI FISSI  ****
051600951110     C                   Z-ADD     1             CODUT
051700960117     C                   Z-ADD     68            KTFP
051800960117     C                   CLEAR                   KTFA
051900951110     C*
052000951110     C* SE SONO IN "P-PARTENZA" I MINUTI DI TOLLERANZA LI REPERISCO
052100951110     C*   DALLA TABELLA "3I"
052200951110     C                   MOVEL     '3I'          COD
052300951110     C                   MOVEL(P)  '1'           KEY
052400951110     C     KTAB          CHAIN     TABEL                              30
052500960123    1C     *IN30         IFEQ      *OFF
052600951110     C                   MOVEL     TBLUNI        DS3I
052700960123   X1C                   ELSE
052800951110     C                   Z-ADD     30            §3IMNA
052900951110     C                   Z-ADD     30            §3IMNR
053000960123    1C                   ENDIF
053100951110     C*
053200951110     C* SE SONO IN "A-ARRIVO" I MINUTI DI TOLLERANZA LI REPERISCO
053300951110     C*   DALLA TABELLA "7A"
053400951110     C                   MOVEL     '7A'          COD
053500951110     C                   MOVEL(P)  '2'           KEY
053600951110     C     KTAB          CHAIN     TABEL                              30
053700960123    1C     *IN30         IFEQ      *OFF
053800951110     C                   MOVEL     TBLUNI        DS7A2
053900960123   X1C                   ELSE
054000951110     C                   Z-ADD     30            §7AMNA
054100951110     C                   Z-ADD     30            §7AMNR
054200960123    1C                   ENDIF
054300960123     C*
054400960117     C* CARICAMENTO TABELLA 1Y - SIMBOLI CALENDARIO BARTOLINI
054500960117     C* (CARICAMENTO DEI SIMBOLI FESTIVI PER PROCDEURA TRAINI)
054600960117     C                   MOVE      *ZEROS        C
054700960117     C                   MOVEL     '1Y'          COD
054800960118     C     KTAB0         SETLL     TABEL00F
054900960118     C     KTAB0         READE     TABEL00F                               30
055000960117     C     *IN30         DOWEQ     *OFF
055100960117     C     TBLFLG        IFEQ      *BLANKS
055200960117     C                   MOVEL     TBLUNI        DS1Y
055300960117     C     §1YFET        IFNE      *BLANKS
055400960117     C                   ADD       1             C
055500960118     C                   MOVEL     TBLKEY        S1Y(C)
055600960117     C                   MOVE      §1YFET        FET(C)
055700960117     C                   END
055800960117     C                   END
055900960118     C     KTAB0         READE     TABEL00F                               30
056000960117     C                   ENDDO
056100960201    1C                   END
056200960117     C*
056300951110     C                   ENDSR
056400951110     C*
056500951110     C*---------------------------------------------------------------*
056600951113**  GFV
056700951110BCDEFGHILMNOPQRSTUVZ
056800951110**  MSG
056900951113Non e' stato trovato nessun orario                                            1
