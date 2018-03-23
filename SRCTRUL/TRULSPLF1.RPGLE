000100030410      /TITLE List Spooled Files
000200030409
000300030409     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400030409
000500030409     FLSplF00F  O    E             DISK
000600030409
000700030410     D UserSpaceName   C                   'LSPLF00F  QTEMP'
000800030409     D Carattere       C                   'C'
000900030409     D Binario         C                   'B'
001000030409
001100030409      /COPY QSYSINC/QRPGLESRC,QUSEC
001200030409      /COPY QSYSINC/QRPGLESRC,QUSGEN
001300030409      /COPY QSYSINC/QRPGLESRC,QUSLSPL
001400030409
001500030409     D WUSSPLFI        DS
001600030409     D                                     INZ
001700030409     D  YUSSPLFI
001800030409     D                                     LIKE(QUSSPLFI)
001900030409     D  QUSKEYS                       9B 0
002000030409     D                                     DIM(24)
002100030409
002200030409     D WUSF0200        DS
002300030409     D                                     INZ
002400030409     D  QUSLFIR03                     9B 0                                      Length of field info
002500030409     D  QUSKFFFR01                    9B 0                                      Key field for field
002600030409     D  QUSTOD03                      1                                         Type of data
002700030409     D  QUSR301                       3                                         Reserved
002800030409     D  QUSDL03                       9B 0                                      Length of data retur
002900030409
003000030410     ** Parametri API QUSCRTUS
003100030409     D CrtUS01         S             20                                         Qualified user space
003200030409     D                                     INZ(UserSpaceName)
003300030409     D CrtUS02         S             10                                         Extended attribute
003400030409     D CrtUS03         S              9B 0                                      Initial size
003500030410     D                                     INZ(4096)
003600030409     D CrtUS04         S              1                                         Initial value
003700030409     D                                     INZ(X'00')
003800030409     D CrtUS05         S             10                                         Public authority
003900030409     D                                     INZ('*EXCLUDE')
004000030409     D CrtUS06         S             50                                         Text description
004100030410     D                                     INZ('List Spooled Files.')
004200030409     D CrtUS07         S             10                                         Replace
004300030409     D                                     INZ('*YES')
004400030409
004500030410     ** Parametri API QUSRTVUS
004600030409     D RtvUS01         S             20                                         Qualified user space
004700030409     D                                     INZ(UserSpaceName)
004800030409     D RtvUS02         S              9B 0                                      Starting position
004900030409     D RtvUS03         S              9B 0                                      Length of data
005000030409
005100030409     ** Parametri API QUSLSPL
005200030409     ** http://publib.boulder.ibm.com/pubs/html/as400/v4r5/ic2924/info/apis/QUSLSPL.htm
005300030409     D LSpl01          S             20                                         Qualified user space
005400030409     D                                     INZ(UserSpaceName)
005500030409     D LSpl02          S              8                                         Format name
005600030409     D                                     INZ('SPLF0200')
005700030409     D LSpl03          S             10                                         User name
005800030409     D LSpl04          S             20                                         Qualified output que
005900030409     D LSpl05          S             10                                         Form type
006000030409     D LSpl06          S             10                                         User-specified data
006100030409     D LSpl08          S             26                                         Qualified job name
006200030409     D LSpl09          S              9B 0                                      Key for the fields t
006300030409     D                                     DIM(%ELEM(QUSKEYS))
006400030409     D LSpl10          S              9B 0                                      Number of fields to
006500030409     D                                     INZ(%ELEM(LSpl09))
006600030409
006700030409     D L               S
006800030409     D                                     LIKE(QUSNBRLE)
006900030409     D F               S
007000030409     D                                     LIKE(QUSNBRFR00)
007100030409
007200030409     D PrmMsgID        S
007300030409     D                                     LIKE(QUSEI)
007400030409     D WrkUS02         S
007500030409     D                                     LIKE(RtvUS02)
007600030409     D WrkChr16        S             16
007700030409     D WrkChr10        S             10
007800030409     D WrkChr7         S              7
007900030409     D WrkChr6         S              6
008000030409     D WrkChr2         S              2
008100030409     D WrkBin4         S              9B 0
008200030409
008300030409     ***********************************************************************
008400030409     **
008500030409     ** Descrizione parametri:
008600030409     **
008700030409     ** ---------- ---------------------------------- ----------
008800030409     **                                               Valori
008900030409     ** Nome       Descrizione                        speciali
009000030409     ** ---------- ---------------------------------- ----------
009100030409     ** LSpl03     User name                          *ALL
009200030409     **                                               *CURRENT
009300030409     ** LSpl04     Qualified output queue name        *ALL
009400030409     ** LSpl05     Form type                          *ALL
009500030409     **                                               *STD
009600030409     ** LSpl06     User-specified data                *ALL
009700030409     ** LSpl08     Qualified job name                 *
009800030410     ** PrmMsgID   Identificativo messaggio di errore
009900030409     **
010000030409     ***********************************************************************
010100030409     C     *ENTRY        PLIST
010200030409     ** Parametri di input.
010300030409     C                   PARM                    LSpl03                         User name
010400030409     C                   PARM                    LSpl04                         Qualified output que
010500030409     C                   PARM                    LSpl05                         Form type
010600030409     C                   PARM                    LSpl06                         User-specified data
010700030409     C                   PARM                    LSpl08                         Qualified job name
010800030409     ** Parametri di output.
010900030409     C                   PARM      QUSEI         PrmMsgID
011000030409
011100030409     ** Creo lo user space.
011200030409     C                   CLEAR                   QUSEC
011300030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
011400030409
011500030409     C                   CALL      'QUSCRTUS'
011600030409     C                   PARM                    CrtUS01
011700030409     C                   PARM                    CrtUS02
011800030409     C                   PARM                    CrtUS03
011900030409     C                   PARM                    CrtUS04
012000030409     C                   PARM                    CrtUS05
012100030409     C                   PARM                    CrtUS06
012200030409     C                   PARM                    CrtUS07
012300030409     C                   PARM                    QUSEC
012400030409
012500030409     C                   IF        QUSEI <> *BLANK
012600030409     C                   EXSR      Uscita
012700030409     C                   ENDIF
012800030409
012900030409     ** Riempio lo user space con la lista degli sppol.
013000030409     C                   CLEAR                   QUSEC
013100030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
013200030409
013300030409     C                   CALL      'QUSLSPL'
013400030409     C                   PARM                    LSpl01
013500030409     C                   PARM                    LSpl02
013600030409     C                   PARM                    LSpl03
013700030409     C                   PARM                    LSpl04
013800030409     C                   PARM                    LSpl05
013900030409     C                   PARM                    LSpl06
014000030409     C                   PARM                    QUSEC
014100030409     C                   PARM                    LSpl08
014200030409     C                   PARM                    LSpl09
014300030409     C                   PARM                    LSpl10
014400030409
014500030409     C                   IF        QUSEI <> *BLANK
014600030409     C                   EXSR      Uscita
014700030409     C                   ENDIF
014800030409
014900030410     ** Retrieve User Space - Generic Header
015000030409     C                   EVAL      RtvUS02 = 1
015100030409     C                   EVAL      RtvUS03 = %LEN(QUSH0100)
015200030409     C                   CLEAR                   QUSEC
015300030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
015400030409
015500030409     C                   CALL      'QUSRTVUS'
015600030409     C                   PARM                    RtvUS01
015700030409     C                   PARM                    RtvUS02
015800030409     C                   PARM                    RtvUS03
015900030409     C                   PARM                    QUSH0100
016000030409     C                   PARM                    QUSEC
016100030409
016200030409     C                   IF        QUSEI <> *BLANK
016300030409     C                   EXSR      Uscita
016400030409     C                   ENDIF
016500030409
016600030410     ** Retrieve User Space - Input Parameter Section
016700030409     C                   EVAL      RtvUS02 = QUSOIP + 1
016800030409     C                   EVAL      RtvUS03 = QUSSIP
016900030409     C                   CLEAR                   QUSEC
017000030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017100030409
017200030409     C                   CALL      'QUSRTVUS'
017300030409     C                   PARM                    RtvUS01
017400030409     C                   PARM                    RtvUS02
017500030409     C                   PARM                    RtvUS03
017600030409     C     QUSSPLFI      PARM                    WUSSPLFI
017700030409     C                   PARM                    QUSEC
017800030409
017900030409     C                   IF        QUSEI <> *BLANK
018000030409     C                   EXSR      Uscita
018100030409     C                   ENDIF
018200030409
018300030410     ** Retrieve User Space - Header Section.
018400030409     C                   EVAL      RtvUS02 = QUSOHS + 1
018500030409     C                   EVAL      RtvUS03 = QUSSHS
018600030409     C                   CLEAR                   QUSEC
018700030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
018800030409
018900030409     C                   CALL      'QUSRTVUS'
019000030409     C                   PARM                    RtvUS01
019100030409     C                   PARM                    RtvUS02
019200030409     C                   PARM                    RtvUS03
019300030409     C                   PARM                    QUSSPLFH
019400030409     C                   PARM                    QUSEC
019500030409
019600030409     C                   IF        QUSEI <> *BLANK
019700030409     C                   EXSR      Uscita
019800030409     C                   ENDIF
019900030409
020000030409     ** Imposto la posizione iniziale della lista.
020100030409     C                   EVAL      RtvUS02 = QUSOLD + 1
020200030409
020300030409     C                   EVAL      L = 1
020400030409     C                   DOW       L <= QUSNBRLE
020500030409
020600030410     ** Retrieve User Space - Number of fields returned
020700030409     C                   EVAL      RtvUS03 = %SIZE(QUSNBRFR00)
020800030409     C                   CLEAR                   QUSEC
020900030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
021000030409
021100030409     C                   CALL      'QUSRTVUS'
021200030409     C                   PARM                    RtvUS01
021300030409     C                   PARM                    RtvUS02
021400030409     C                   PARM                    RtvUS03
021500030409     C                   PARM                    QUSNBRFR00
021600030409     C                   PARM                    QUSEC
021700030409
021800030409     C                   IF        QUSEI <> *BLANK
021900030409     C                   EXSR      Uscita
022000030409     C                   ENDIF
022100030410
022200030410     ** A questo punto nel campo QUSNBRFR00 c'è il numero di campi
022300030410     ** restituiti, che è 24 (key da 201 a 224).
022400030409
022500030409     C                   EVAL      RtvUS02 = RtvUS02 + RtvUS03
022600030409     C                   EVAL      F = 1
022700030409
022800030409     C                   DOW       F <= QUSNBRFR00
022900030409
023000030410     ** Retrieve User Space - Dati.
023100030409     C                   EVAL      RtvUS03 = %SIZE(WUSF0200)
023200030409     C                   CLEAR                   QUSEC
023300030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
023400030409
023500030409     C                   CALL      'QUSRTVUS'
023600030409     C                   PARM                    RtvUS01
023700030409     C                   PARM                    RtvUS02
023800030409     C                   PARM                    RtvUS03
023900030409     C                   PARM                    WUSF0200
024000030409     C                   PARM                    QUSEC
024100030409
024200030409     C                   IF        QUSEI <> *BLANK
024300030409     C                   EXSR      Uscita
024400030409     C                   ENDIF
024500030409
024600030409     ** Determino la posizione iniziale del prossimo key field.
024700030409     C                   EVAL      WrkUS02 = RtvUS02 + QUSLFIR03
024800030409
024900030409     C                   EXSR      RtvData
025000030409
025100030409     ** Imposto la posizione iniziale del prossimo key field.
025200030409     C                   EVAL      RtvUS02 = WrkUs02
025300030409     C                   EVAL      F = F + 1
025400030409
025500030409     C                   ENDDO
025600030409
025700030410     C                   EXSR      WrtLSplF00F
025800030409
025900030409     ** Determino la posizione iniziale del prossimo elemento.
026000030409     C                   EVAL      RtvUS02 = QUSOLD + 1 + (QUSSEE * L)
026100030409
026200030409     C                   EVAL      L = L + 1
026300030409
026400030409     C                   ENDDO
026500030409
026600030409     C                   EXSR      Uscita
026700030409
026800030409     ***********************************************************************
026900030409     **
027000030409     ** Reperisco il dato corrispondente al key.
027100030409     **
027200030409     ***********************************************************************
027300030409     C     RtvData       BEGSR
027400030410
027500030410     ** A questo punto nel campo QUSTOD03 c'è il tipo di dati ritornato
027600030410     ** e in QUSDL03 c'è la lunghezza in byte.
027700030409
027800030409     ** Retrieve User Space (QUSRTVUS) API - Dati.
027900030409     C                   EVAL      RtvUS02 = RtvUS02 + RtvUS03
028000030409     C                   EVAL      RtvUS03 = QUSDL03
028100030409     C                   CLEAR                   QUSEC
028200030409     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028300030409
028400030409     C                   SELECT
028500030409
028600030409     C                   WHEN      QUSTOD03 = Carattere
028700030409     C                             AND
028800030409     C                             QUSDL03 = 10
028900030409
029000030409     C                   CALL      'QUSRTVUS'
029100030409     C                   PARM                    RtvUS01
029200030409     C                   PARM                    RtvUS02
029300030409     C                   PARM                    RtvUS03
029400030409     C                   PARM                    WrkChr10
029500030409     C                   PARM                    QUSEC
029600030409
029700030409     C                   WHEN      QUSTOD03 = Binario
029800030409     C                             AND
029900030409     C                             QUSDL03 = 4
030000030409
030100030409     C                   CALL      'QUSRTVUS'
030200030409     C                   PARM                    RtvUS01
030300030409     C                   PARM                    RtvUS02
030400030409     C                   PARM                    RtvUS03
030500030409     C                   PARM                    WrkBin4
030600030409     C                   PARM                    QUSEC
030700030409
030800030409     C                   WHEN      QUSTOD03 = Carattere
030900030409     C                             AND
031000030409     C                             QUSDL03 = 6
031100030409
031200030409     C                   CALL      'QUSRTVUS'
031300030409     C                   PARM                    RtvUS01
031400030409     C                   PARM                    RtvUS02
031500030409     C                   PARM                    RtvUS03
031600030409     C                   PARM                    WrkChr6
031700030409     C                   PARM                    QUSEC
031800030409
031900030409     C                   WHEN      QUSTOD03 = Carattere
032000030409     C                             AND
032100030409     C                             QUSDL03 = 7
032200030409
032300030409     C                   CALL      'QUSRTVUS'
032400030409     C                   PARM                    RtvUS01
032500030409     C                   PARM                    RtvUS02
032600030409     C                   PARM                    RtvUS03
032700030409     C                   PARM                    WrkChr7
032800030409     C                   PARM                    QUSEC
032900030409
033000030409     C                   WHEN      QUSTOD03 = Carattere
033100030409     C                             AND
033200030409     C                             QUSDL03 = 16
033300030409
033400030409     C                   CALL      'QUSRTVUS'
033500030409     C                   PARM                    RtvUS01
033600030409     C                   PARM                    RtvUS02
033700030409     C                   PARM                    RtvUS03
033800030409     C                   PARM                    WrkChr16
033900030409     C                   PARM                    QUSEC
034000030409
034100030409     C                   WHEN      QUSTOD03 = Carattere
034200030409     C                             AND
034300030409     C                             QUSDL03 = 2
034400030409
034500030409     C                   CALL      'QUSRTVUS'
034600030409     C                   PARM                    RtvUS01
034700030409     C                   PARM                    RtvUS02
034800030409     C                   PARM                    RtvUS03
034900030409     C                   PARM                    WrkChr2
035000030409     C                   PARM                    QUSEC
035100030409
035200030409     C                   ENDSL
035300030409
035400030409     C                   IF        QUSEI <> *BLANK
035500030409     C                   EXSR      Uscita
035600030409     C                   ENDIF
035700030409
035800030410     C                   EXSR      SetLSplF00F
035900030409
036000030409     C                   ENDSR
036100030409
036200030409     ***********************************************************************
036300030409     **
036400030410     ** Impostazione campi file LSPLF00F.
036500030409     **
036600030409     ** ---------- ----- ----- - -- ---------------------------------------
036700030409     ** Nome Campo P.In. Lung.TC DC Testo
036800030409     ** ---------- ----- ----- - -- ---------------------------------------
036900030410     ** LSPLKEY201     1    10 A    Spooled file name
037000030410     ** LSPLKEY202    11    10 A    Job name
037100030410     ** LSPLKEY203    21    10 A    User name
037200030410     ** LSPLKEY204    31     6 A    Job number
037300030410     ** LSPLKEY205    37     9 B  0 Spooled file number
037400030410     ** LSPLKEY206    41    10 A    Output queue name
037500030410     ** LSPLKEY207    51    10 A    Output queue library name
037600030410     ** LSPLKEY208    61    10 A    Device
037700030410     ** LSPLKEY209    71    10 A    User-specified data
037800030410     ** LSPLKEY210    81    10 A    Status
037900030410     ** LSPLKEY211    91     9 B  0 Total pages
038000030410     ** LSPLKEY212    95     9 B  0 Current pages
038100030410     ** LSPLKEY213    99     9 B  0 Copies left to produce
038200030410     ** LSPLKEY214   103    10 A    Form type
038300030410     ** LSPLKEY215   113     2 A    Priority
038400030415     ** LSPLKEY216   115    10 L    Date file was opened
038500030415     ** LSPLKEY217   122     8 T    Time file was opened
038600030410     ** LSPLKEY218   128    16 A    Internal job identifier
038700030410     ** LSPLKEY219   144    16 A    Internal spooled file identifier
038800030410     ** LSPLKEY220   160    10 A    Device type
038900030415     ** LSPLKEY221   170    10 L    Date file last used
039000030410     ** LSPLKEY222   177     9 B  0 Spooled file size multiplier
039100030410     ** LSPLKEY223   181     9 B  0 Size of spooled file
039200030410     ** LSPLKEY224   185     9 B  0 Auxiliary storage pool
039300030409     **
039400030409     ***********************************************************************
039500030410     C     SetLSplF00F   BEGSR
039600030409
039700030409     C                   SELECT
039800030409     C                   WHEN      QUSKFFFR01 = 201
039900030410     C                   EVAL      LSplKey201 = WrkChr10
040000030409     C                   WHEN      QUSKFFFR01 = 202
040100030410     C                   EVAL      LSplKey202 = WrkChr10
040200030409     C                   WHEN      QUSKFFFR01 = 203
040300030410     C                   EVAL      LSplKey203 = WrkChr10
040400030409     C                   WHEN      QUSKFFFR01 = 204
040500030410     C                   EVAL      LSplKey204 = WrkChr6
040600030409     C                   WHEN      QUSKFFFR01 = 205
040700030410     C                   EVAL      LSplKey205 = WrkBin4
040800030409     C                   WHEN      QUSKFFFR01 = 206
040900030410     C                   EVAL      LSplKey206 = WrkChr10
041000030409     C                   WHEN      QUSKFFFR01 = 207
041100030410     C                   EVAL      LSplKey207 = WrkChr10
041200030409     C                   WHEN      QUSKFFFR01 = 208
041300030410     C                   EVAL      LSplKey208 = WrkChr10
041400030409     C                   WHEN      QUSKFFFR01 = 209
041500030410     C                   EVAL      LSplKey209 = WrkChr10
041600030409     C                   WHEN      QUSKFFFR01 = 210
041700030410     C                   EVAL      LSplKey210 = WrkChr10
041800030409     C                   WHEN      QUSKFFFR01 = 211
041900030410     C                   EVAL      LSplKey211 = WrkBin4
042000030409     C                   WHEN      QUSKFFFR01 = 212
042100030410     C                   EVAL      LSplKey212 = WrkBin4
042200030409     C                   WHEN      QUSKFFFR01 = 213
042300030410     C                   EVAL      LSplKey213 = WrkBin4
042400030409     C                   WHEN      QUSKFFFR01 = 214
042500030410     C                   EVAL      LSplKey214 = WrkChr10
042600030409     C                   WHEN      QUSKFFFR01 = 215
042700030410     C                   EVAL      LSplKey215 = WrkChr2
042800030409     C                   WHEN      QUSKFFFR01 = 216
042900030415     C                   IF        WrkChr7 <> *BLANK
043000030415     C     *CYMD0        MOVE      WrkChr7       LSplKey216
043100030415     C                   ELSE
043200030415     C                   CLEAR                   LSplKey216
043300030415     C                   ENDIF
043400030409     C                   WHEN      QUSKFFFR01 = 217
043500030415     C                   IF        WrkChr6 <> *BLANK
043600030415     C     *ISO0         MOVE      WrkChr6       LSplKey217
043700030415     C                   ELSE
043800030415     C                   CLEAR                   LSplKey217
043900030415     C                   ENDIF
044000030409     C                   WHEN      QUSKFFFR01 = 218
044100030410     C                   EVAL      LSplKey218 = WrkChr16
044200030409     C                   WHEN      QUSKFFFR01 = 219
044300030410     C                   EVAL      LSplKey219 = WrkChr16
044400030409     C                   WHEN      QUSKFFFR01 = 220
044500030410     C                   EVAL      LSplKey220 = WrkChr10
044600030409     C                   WHEN      QUSKFFFR01 = 221
044700030415     C                   IF        WrkChr7 <> *BLANK
044800030415     C     *CYMD0        MOVE      WrkChr7       LSplKey221
044900030415     C                   ELSE
045000030415     C                   CLEAR                   LSplKey221
045100030415     C                   ENDIF
045200030409     C                   WHEN      QUSKFFFR01 = 222
045300030410     C                   EVAL      LSplKey222 = WrkBin4
045400030409     C                   WHEN      QUSKFFFR01 = 223
045500030410     C                   EVAL      LSplKey223 = WrkBin4
045600030409     C                   WHEN      QUSKFFFR01 = 224
045700030410     C                   EVAL      LSplKey224 = WrkBin4
045800030409     C                   ENDSL
045900030409
046000030409     C                   ENDSR
046100030409
046200030409     ***********************************************************************
046300030409     **
046400030410     ** Scrittura LSPLF00F.
046500030409     **
046600030409     ***********************************************************************
046700030410     C     WrtLSplF00F   BEGSR
046800030409
046900030409     C                   WRITE     LSplF000
047000030409
047100030409     C                   ENDSR
047200030409
047300030409     ***********************************************************************
047400030409     **
047500030409     ** Operazioni finali.
047600030409     **
047700030409     ***********************************************************************
047800030409     C     Uscita        BEGSR
047900030409
048000030409     C                   EVAL      *INLR = *ON
048100030409     C                   RETURN
048200030409
048300030409     C                   ENDSR
048400030409
048500030409     ***********************************************************************
048600030409     **
048700030409     ** Operazioni iniziali.
048800030409     **
048900030409     ***********************************************************************
049000030409     C     *INZSR        BEGSR
049100030409
049200030409     ** Inizializzo la schiera con i valid keys (201-224).
049300030409     ** Ogni key corrisponde ad una informazione (es. 201 = Spooled file name)
049400030409     C                   DO        24            L
049500030409     C                   EVAL      LSpl09(L) = 200 + L
049600030409     C                   ENDDO
049700030409
049800030409     C                   ENDSR
