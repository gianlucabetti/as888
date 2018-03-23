000100971203     H*------------------------------------------------------------------------*
000200971203     H* RICERCA CODICI ORGANIGRAMMA
000300971203     H* - AGGIUNTA LA PARZIALIZZAZIONE SU ORGDIT ('SDI/BAR/'   '=TUTTE)
000400971203     H*------------------------------------------------------------------------*
000500971203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000600971203     F*------------------------------------------------------------------------*
000700971203     F* DATA BASE
000800971203     F*------------------------------------------------------------------------*
000900900612     FAZORG03L  IF   E           K DISK
001000910225     FAZOSE03L  IF   E           K DISK
001100910225     F                                     RENAME(AZORG:AZOSE)
001200971203     FTNSD24D   CF   E             WORKSTN
001300971203     F                                     SFILE(SD24DSF:REC1)
001400020531     d*--------------------------------------------------------------------------------------------*
001500020531     d* Data structure                                                                             *
001600020531     d*--------------------------------------------------------------------------------------------*
001700020603     d OG143         E DS
001800020603      *
001900020531     d NotUsed         S             16A
002000020531     d ReturnSize      S              9B 0
002100020531     d SizeList        S              9B 0
002200020531     d WrkSort         S              1
002300020531     d*---
002400020531     d Rec1            S              3  0                                      *n° rel sf
002500020531     d stmrec1         S              3  0                                      *n° rel sf x stampa
002600020531     d*---
002700020531     d MaxKey          C                   2
002800020531     d Ascendente      C                   1
002900020531     d Discendente     C                   2
003000020531     d Carattere       C                   6
003100020531     d Put             C                   1
003200020531     d EndPut          C                   2
003300020531     d Get             C                   3
003400020603     d Numerico        C                   8
003500020531     d*---
003600020531     d SflRcd          DS
003700020531     d  Scelta
003800020603     d  OrgTip
003900020531     d  OrgFil
004000020531     d  OrgDes
004100020725     d  OrgNtw
004200020531     d  OrgFva
004300020531     d  Selected                      1A
004400020531
004500020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
004600020531     d QLGKL                         16    DIM(MaxKey)
004700020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
004800020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
004900020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
005000020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
005100020531
005200020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
005300020531      /COPY QSYSINC/QRPGLESRC,QUSEC
005400971203     C*------------------------------------------------------------------------*
005500971203     C* MAIN LINE
005600971203     C*------------------------------------------------------------------------*
005700971203     C     *ENTRY        PLIST
005800971203     C                   PARM                    §COD
005900971203     C                   PARM                    §TIP
006000971203     C                   PARM                    §DES             25
006100971203     C                   PARM                    §PRA              3
006200910225     C     §TIP          COMP      '§'                                    88
006300900418     C     *LIKE         DEFINE    ORGFAG        §TIP
006400020604     C     *LIKE         DEFINE    C1CPOU        W1CPOU
006500020604     C     *LIKE         DEFINE    C1CDES        W1CDES
006600900418     C                   MOVE      *BLANKS       SCELTA                         *
006700900418     C                   Z-ADD     0             REC1              3 0
006800900418     C*---------- PULIZIA SLFCT :                          *
006900900418     C                   SETON                                            70    *
007000971203     C                   WRITE     SD24DCT                                      *
007100900418     C                   SETOFF                                       7071      *
007200900418     C*----------
007300021128      * se richiesto un tipo non imposto la descrizione ad 'A'
007400040518     C                   IF        §tip =  *blanks and §DES=*blanks
007500020709     C                   movel     'A'           C1cdes
007600040518     c                   else
007700040518     C                   IF        §tip =  *blanks
007800040518     c                   movel(P)  §des          c1cdes
007900021128     C                   ENDIF
008000040518     C                   ENDIF
008100040518     c
008200020710     C                   move      *on           $prima_emis       1
008300020710     C                   setoff                                       15
008400020531     C                   Z-ADD     *zeros        Rec1                           *n° record sfile
008500971203     C                   WRITE     SD24D1                                       *
008600020709      *
008700900418     C     SOPRA         TAG                                                    *
008800020604      *
008900020531      * Organigramma aziendale di sede
009000020709     C   88              READ      AZOSE                                  91
009100020604      *
009200020531      * Organigramma aziendale di azienda/filiale
009300020709     C  N88              READ      AZORG                                  91
009400020617      *
009500020617     C   91              GOTO      EMISF1                                       *
009600020606      *
009700020531      * a seconda del tipo passato in input seleziono i record
009800020531     C     §TIP          IFNE      *BLANKS
009900020531     C                   EXSR      SELTIP
010000020531     C     $NoSel        IFEQ      *ON
010100020531     C                   GOTO      SOPRA                                        *
010200020531     C                   ENDIF
010300020604     C                   ENDIF
010400020531      *
010500971203     C     §PRA          IFNE      *BLANKS
010600971203     C     §PRA          ANDNE     ORGDIT
010700971203     C                   GOTO      SOPRA                                        *
010800971203     C                   ENDIF
010900971203     C*
011000020613     C*---
011100900418     C     REC1          ADD       1             REC1                 71        *
011200020603     C                   EXSR      RIESFL
011300020613     C                   WRITE     SD24Z1                                       *
011400971203     C                   WRITE     SD24DSF                                      *
011500020603     C                   Z-ADD     REC1          STMREC1                        *n° rel sf x stampa
011600020710      *
011700020710      * alla pima emissione mi posiziono sempre a partire da 'A'
011800021128      * a meno che non abbia richiesto il tipo p.o
011900120206      *          o non abbia ricevuto un altro posizionamento
012000021128     C                   if        $prima_emis = *off or §tip <> *blanks
012100020710     C                   EVAL      RcdNbr  = 1
012200020710     C                   else
012300120206   3aC*//                IF        *in15 = *off and 'A' <= OrgDes
012400120206   3aC                   IF        *in15 = *off and C1Cdes <= OrgDes
012500020710     C                   EVAL      RcdNbr  = Rec1
012600020710     C                   seton                                        15
012700020710     C                   endif
012800020710     C                   endif
012900000000     C                   GOTO      SOPRA                                        *
013000000000     C*---------- EMISSIONE SFL :                          *
013100020710     C                   move      *off          $prima_emis
013200000000     C     EMISF1        TAG                                                    *
013300020709     C                   EXFMT     SD24DCT                                      *
013400020709     C   KL              SETON                                            90    *
013500020709     C   KL              GOTO      FINE                                         *
013600020604      *
013700020603     C* F11 = Cambia ordinamento
013800020603if  1C                   IF        *INKK
013900020603     C                   EXSR      F11GES
014000020603e   1C                   ENDIF
014100020604      *
014200020606     C                   EXSR      CTR
014300000000     C*----------------------------------------------------*
014400000000     C     FINE          TAG                                                    *
014500020603      *
014600020603      * Non ho scelto nulla
014700900418     C   90              MOVE      *BLANKS       §COD              3            *
014800900515     C   90              MOVE      *BLANKS       §DES                           *
014900020603     C   90              MOVE      *BLANKS       §TIP                           *
015000000000     C                   SETON                                            LR    *
015100020531      *--------------------------------------------------------------------------------------------*
015200020531      * Selezione Tipo P.O
015300020531      *--------------------------------------------------------------------------------------------*
015400020531     C     SELTIP        BEGSR
015500020531     c                   move      ' '           $NoSel            1
015600020531     C                   SELECT
015700020531      * Filiale
015800020531     C     §TIP          WHENEQ    'F'
015900020531     C                   if        OrgFag <>'F'
016000020531     C                   move      '1'           $NoSel
016100020531     C                   endif
016200020531      * Agenzia
016300020531     C     §TIP          WHENEQ    'A'
016400020531     C                   if        OrgFag <>'A'
016500020531     C                   move      '1'           $NoSel
016600020531     C                   endif
016700020531      * Agenzia/Filiale
016800020531     C     §TIP          WHENEQ    'E'
016900020603     C                   if        OrgFag <>'F' and OrgFag <>'A'
017000020531     C                   move      '1'           $NoSel
017100020531     C                   endif
017200020531      * Varie
017300020531     C     §TIP          WHENEQ    'V'
017400020531     C                   if        OrgFag <>'V' or OrgFl2 <>' '
017500020531     C                   move      '1'           $NoSel
017600020531     C                   endif
017700020531      * Linea per tariffe
017800020531     C     §TIP          WHENEQ    'T'
017900020531     C                   if        OrgFag <>'V' or OrgFl2 <>'S'
018000020531     C                   move      '1'           $NoSel
018100020531     C                   endif
018200020531      * Varie piu' linea per tariffe
018300020531     C     §TIP          WHENEQ    'W'
018400020531     C                   if        OrgFag <>'V' or OrgFl2 <>' '
018500020531     C                             or OrgFl2<> 'S'
018600020531     C                   move      '1'           $NoSel
018700020531     C                   endif
018800020531     C                   ENDSL
018900020531     C                   ENDSR
019000020603      *--------------------------------------------------------------------------------------------*
019100020603      * Valorizzo campo tipo flag tipo p.o
019200020603      *--------------------------------------------------------------------------------------------*
019300020603     C     RIESFL        BEGSR
019400020603      *
019500020603      * TIPO P.O.
019600020603     C                   SELECT
019700020603      * Filiale
019800020603     C                   WHEN      OrgFag = 'F'
019900020603     C                   move      'F'           OrgTip
020000020603      * Agenzia
020100020603     C                   WHEN      OrgFag = 'A'
020200020603     C                   move      'A'           OrgTip
020300020603      * Varie
020400020603     C                   WHEN      OrgFag = 'V' and OrgFl2 =' '
020500020603     C                   move      'V'           OrgTip
020600020603      * Linea per tariffa
020700020603     C                   WHEN      OrgFag = 'V' and OrgFl2 ='S'
020800020603     C                   move      'T'           OrgTip
020900020603     C                   ENDSL
021000020603      *
021100020603      * NETWORK
021200020603     C                   MOVEL     OrgDe3        OG143
021300020603     C                   MOVEL     §ogntw        ORGNTW
021400020603      *
021500020603     C                   ENDSR
021600020531      *--------------------------------------------------------------------------------------------*
021700020531      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
021800020531      *--------------------------------------------------------------------------------------------*
021900020531     C     F11GES        BEGSR
022000020710     C                   setoff                                       72
022100020613     C                   CLEAR                   C1CPou
022200020709      * IMPOSTO ENTRATA AD 'A'
022300020613     C                   EVAL      rcdnbr  = 1
022400020531      *
022500020531     C                   CLEAR                   QLGSCB
022600020531     C                   CLEAR                   QLGSCB00
022700020531
022800020531     ** 1 campo chiave.
022900020531     C                   EVAL      QLGNBRK    = 1
023000020531
023100020603     C                   IF        *in50=*off
023200020603     ** IL P.O è a posizione 3, numerico, 3 byte, ascending.
023300020603     C                   EVAL      QLGSP      = 3
023400020603     C                   EVAL      QLGSS      = %SIZE(ORGFIL)
023500020603     C                   EVAL      QLGDT      = Numerico
023600020531     C                   EVAL      QLGSO      = Ascendente
023700020531     C                   EVAL      QLGKL(1)   = QLGSKL
023800020603     C                   seton                                        50
023900020603     C                   ELSE
024000020603      *
024100020603     ** La ragione sociale è a posizione 6, 20 byte, char, ascending.
024200020603     C                   EVAL      QLGSP      = 6
024300020603     C                   EVAL      QLGSS      = %SIZE(ORGDES)
024400020603     C                   EVAL      QLGDT      = Carattere
024500020603     C                   EVAL      QLGSO      = Ascendente
024600020603     C                   EVAL      QLGKL(1)   = QLGSKL
024700020603     C                   setoff                                       50
024800020603     C                   ENDIF
024900020531
025000020531      * Load other sort parameters.
025100020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
025200020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
025300020531     C                   EVAL      QLGRT     = 8
025400020531     C                   EVAL      QLGOKL    = 80
025500020531     C                   EVAL      QLGLKE    = 16
025600020531     C                   EVAL      QLGLSS    = 290
025700020531
025800020531      * Initialize Sort I/O API fields.
025900020531     C                   EVAL      QLGRL00  = QLGRL
026000020531     C                   EVAL      QLGRC00  = 1
026100020531     C                   CLEAR                   QUSEI
026200020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
026300020531
026400020531      * First step - Initialize the sort routine.
026500020531     C                   CALL      'QLGSORT'
026600020531     C                   PARM                    QLGSCB
026700020531     C                   PARM                    NotUsed
026800020531     C                   PARM                    NotUsed
026900020531     C                   PARM                    SizeList
027000020531     C                   PARM                    ReturnSize
027100020531     C                   PARM                    QUSEC
027200020531
027300020531      * Next step - Write records to I/O routine.
027400020531     C                   EVAL      QLGRT00 = Put
027500020531
027600020613     C                   DO        STMREC1       x                 3 0
027700020531
027800020613     C     x             CHAIN     SD24DSF
027900020531
028000020531     ** Solo le righe con Selected = 'Y' sono riordinate,
028100020531     ** quindi per fare un ordinamento di tutte le righe
028200020531     ** metto 'Y' sempre.
028300020531     C                   EVAL      Selected = 'Y'
028400020531
028500020531     C                   CLEAR                   QUSEI
028600020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028700020531     C                   CALL      'QLGSRTIO'
028800020531     C                   PARM                    QLGSCB00
028900020531     C                   PARM                    SflRcd
029000020531     C                   PARM                    NotUsed
029100020531     C                   PARM                    SizeList
029200020531     C                   PARM                    NotUsed
029300020531     C                   PARM                    QUSEC
029400020531
029500020531     C                   ENDDO
029600020531
029700020531      * Next step - Signal end of input, clear subfile for reload.
029800020531     C                   EVAL      QLGRT00 = EndPut
029900020531     C                   CLEAR                   QUSEI
030000020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030100020531
030200020531     C                   CALL      'QLGSRTIO'
030300020531     C                   PARM                    QLGSCB00
030400020531     C                   PARM                    SflRcd
030500020531     C                   PARM                    NotUsed
030600020531     C                   PARM                    SizeList
030700020531     C                   PARM                    NotUsed
030800020531     C                   PARM                    QUSEC
030900020531
031000020531     C                   SETON                                            70    *
031100020531     C                   WRITE     SD24DCT                                      *
031200020603     C                   SETOFF                                       70        *
031300020612     C                   WRITE     SD24D1                                       *
031400020613     C                   WRITE     SD24Z1                                       *
031500020531
031600020531      * Final step - Write the records back to the subfile.
031700020531     C                   EVAL      QLGRT00 = Get
031800020531
031900020613     C                   DO        STMREC1       x
032000020531     C                   CLEAR                   QUSEI
032100020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
032200020531     C                   CALL      'QLGSRTIO'
032300020531     C                   PARM                    QLGSCB00
032400020531     C                   PARM                    NotUsed
032500020531     C                   PARM                    SflRcd
032600020531     C                   PARM                    QLGRL00
032700020531     C                   PARM                    NotUsed
032800020531     C                   PARM                    QUSEC
032900020531      *
033000020613     c                   z-add     x             rec1
033100020531     C                   WRITE     SD24DSF
033200020531     C                   ENDDO
033300020531
033400020531     C                   EVAL      *IN70 = *off
033500020604      *
033600020531     C                   ENDSR
033700020606      *--------------------------------------------------------------------------------------------*
033800020613      * CTR Subroutine per il riposizionamento
033900020606      *--------------------------------------------------------------------------------------------*
034000020606     C     CTR           BEGSR
034100020606     C*
034200020612     C  N71              GOTO      EMISF1
034300020612     C*
034400020613     C   71              READC     SD24DSF                                91    *
034500020613   1aC     Scelta        IFEQ      '1'                                          *
034600020613     C                   MOVEL     ORGFIL        §COD                           *
034700020613     C                   MOVEL     ORGTIP        §TIP                           *
034800020613     C                   MOVEL     ORGDES        §DES                           *
034900020613   1eC                   ELSE
035000020613      *
035100020613      * Controllo la scelta
035200020710     C                   seton                                        72
035300020613   2aC                   DO        *hival        X
035400020613     C     X             CHAIN     SD24DSF                            91        *
035500020613     C   91              LEAVE
035600020612      *
035700020613   3aC                   IF        (*in50 = *off and C1CDes <= OrgDes) or
035800020613     C                             (*in50 = *on  and C1CPou <= OrgFil)
035900020613     C                   z-add     X             RcdNbr
036000020710     C                   setoff                                       72
036100020613   3eC                   ELSE
036200020613     C                   ITER
036300020613     C                   ENDIF
036400020710     C                   WRITE     SD24Z1                                       *
036500020613     C                   GOTO      EMISF1
036600020613   2-C                   ENDDO
036700020710      * riemetto se non ho trovato records
036800020710     C   72              DO
036900020710     C                   WRITE     SD24Z1                                       *
037000020710     C                   GOTO      EMISF1
037100020710     C                   ENDDO
037200020710   1-C                   ENDIF
037300020606      *
037400020613     C                   move      *blanks       C1CDes
037500020613     C                   move      *blanks       C1CPou
037600020613      *
037700020606     C                   ENDSR
