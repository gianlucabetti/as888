000100000000      **********************************************************************
000200011113      * Author: Danilo Cussini
000300000000      * Program:      SORTSFL
000400000000      * Description:  This program is an example of how to use the sort
000500000000      *               APIs to sort a subfile.
000600000000      **********************************************************************
000700000000
000800000000      **********************************************************************
000900000000      * Indicators used.
001000000000      *
001100011114      * KC - F3 pressed on the display. This causes the program to exit.
001200011114      * KE - F5 ricarica il subfile rileggendo il file.
001300011114      * KK - F11 cambia l'ordine del subfile.
001400011115      * 01 - Colore testata colonna codice tabella.
001500011115      * 02 - Colore testata colonna descrizione.
001600011115      * 03 - Colore testata colonna gruppo.
001700000000      * 30 - *ON  = Display subfile control record.
001800000000      *      *OFF = Clear the subfile.
001900000000      * 31 - *ON  = Subfile contains records.
002000000000      *      *OFF = Subfile does not contain records.
002100000000      *
002200000000      **********************************************************************
002300000000
002400000000      **********************************************************************
002500000000      * Files used
002600000000      *
002700011115      * SORTSFLD - Display file containing subfile to sort.
002800000000      *
002900000000      **********************************************************************
003000011115     FSortSflD  CF   E             WORKSTN SFILE(S01:Rrn1)
003100011115     FTabel00F  IF   E           K DISK
003200011113     F                                     USROPN
003300000000
003400000000      **********************************************************************
003500000000      * Constants
003600000000      *
003700000000      *     MaxKey - Maximum number of key fields allowed by this program
003800000000      *
003900000000      **********************************************************************
004000011115     D MaxKey          C                    2
004100011114     D Descrizione     C                   '1'
004200011115     D Gruppo          C                   '2'
004300011115     D Ascendente      C                   1
004400011115     D Discendente     C                   2
004500011115     D Carattere       C                   6
004600011115     D Put             C                   1
004700011115     D EndPut          C                   2
004800011115     D Get             C                   3
004900000000
005000000000      **********************************************************************
005100000000      * Standalone fields
005200000000      *
005300000000      *     RRN1       - Relative record number for subfile
005400000000      *     SizeList   - Size of list
005500000000      *     ReturnSize - Size of list returned by sort API
005600000000      **********************************************************************
005700011115     D NotUsed         S             16A
005800011115     D ReturnSize      S              9B 0
005900011115     D Rrn1            S              5I 0
006000011115     D SizeList        S              9B 0
006100011115     D RrnLast         S              5I 0
006200011114     D WrkSort         S              1
006300000000
006400000000      **********************************************************************
006500000000      * Data Structures
006600000000      *
006700000000      *     SflRcd     - The entire subfile record to pass to the sort
006800011115      *     QLGSCB     - The sort request block for the QLGSORT API
006900011115      *     QLGSCB00   - The sort request block for the QLGSRTIO API
007000011115      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
007100011115      *     QUSEC      - Error structure for the QLGSORT API
007200000000      **********************************************************************
007300011115     D SflRcd          DS
007400011115     D  S01Cod
007500011115     D  S01Grp
007600011115     D  S01Des
007700011115     D  S01Pgm
007800000000     D  Selected                      1A
007900011115
008000011115      /COPY QSYSINC/QRPGLESRC,QLGSORT
008100011115     D QLGKL                         16    DIM(MaxKey)
008200011115     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
008300011115     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
008400011115     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
008500011115     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008600011115
008700011115      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008800011115      /COPY QSYSINC/QRPGLESRC,QUSEC
008900000000
009000000000      * First call the Initialize subroutine to load records into the
009100011115      * subfile
009200000000     C                   EXSR      Initialize
009300000000
009400011115     C                   WRITE     T01
009500011115     C                   WRITE     P01
009600011115
009700000000      * Now keep displaying the control record and subfile until the
009800000000      * user presses the Cmd key. Sort the subfile records after each
009900011115      * F11 key has been pressed.
010000011115     C                   DOW       NOT *INKC
010100011115
010200011113     C                   EXFMT     C01
010300000000
010400011115     C                   SELECT
010500011115     C                   WHEN      *INKE
010600011114     C                   EXSR      Initialize
010700011115     C                   WHEN      *INKK
010800011113     C                   EXSR      Sort
010900011115     C                   ENDSL
011000000000
011100000000     C                   ENDDO
011200000000
011300000000     C                   EVAL      *INLR = *ON
011400000000     C                   RETURN
011500000000
011600000000      **********************************************************************
011700000000      * Subroutine - Initialize
011800000000      *   This subroutine loads the subfile with the initial data
011900000000     C     Initialize    BEGSR
012000011114
012100011114     C                   EVAL      *IN30 = *OFF
012200011114     C                   EVAL      *IN31 = *OFF
012300011114     C                   WRITE     C01
012400011114     C                   EVAL      *IN30 = *ON
012500000000     C                   EVAL      *IN31 = *ON
012600011114     C                   CLEAR                   Rrn1
012700011115
012800011115     C                   OPEN      Tabel00F
012900011115
013000011115     ** Carico il subfile in ordine di codice filiale.
013100011115     C     K02Tab00      SETLL     Tabel00F
013200011115     C                   DOU       %EOF(Tabel00F)
013300011115     C     K02Tab00      READE     Tabel00F
013400011115
013500011115     C                   IF        NOT %EOF(Tabel00F)
013600011115     C                             AND
013700011115     C                             %SUBST(TblKey:7) <> *BLANK
013800011115     C                   EVAL      S01Cod = %SUBST(TblKey:7)
013900011115     C                   EVAL      S01Des = %SUBST(TblUni:1)
014000011115     C                   EVAL      S01Grp = %SUBST(TblUni:69)
014100011115     C                   EVAL      S01Pgm = %SUBST(TblUni:61:8)
014200011113     C                   EVAL      Rrn1 = Rrn1 + 1
014300011113     C                   WRITE     S01
014400011113     C                   ENDIF
014500011115
014600011113     C                   ENDDO
014700011115
014800011115     C                   CLOSE     Tabel00F
014900011115
015000011114     C                   EVAL      RrnLast = Rrn1
015100011115
015200011114     C                   EVAL      *IN01 = *ON
015300011114     C                   EVAL      *IN02 = *OFF
015400011114     C                   EVAL      *IN03 = *OFF
015500011115
015600011115     ** Al primo F11 ordino per descrizione.
015700011114     C                   EVAL      WrkSort = Descrizione
015800000000
015900000000      * All done initializing.
016000000000     C                   ENDSR
016100000000
016200000000      **********************************************************************
016300011113      * Subroutine - Sort
016400000000      *   This subroutine sorts the subfile records.
016500011113     C     Sort          BEGSR
016600011114
016700011114      * Initialize the key fields to sort on. There is one extra field not in
016800011114      * the subfile -- Selected -- that is added to the record. This field
016900011114      * is used to place selected records in front of nonselected records.
017000011115
017100011115     C                   CLEAR                   QLGSCB
017200011115     C                   CLEAR                   QLGSCB00
017300011114
017400011115     ** Gestione della scelta ordinamento.
017500011114     C                   SELECT
017600011115     ** Ordinamento per descrizione.
017700011114     C                   WHEN      WrkSort = Descrizione
017800011115
017900011115     ** 1 campo chiave.
018000011115     C                   EVAL      QLGNBRK    = 1
018100011115
018200011115     ** La descrizione è
018300011115     ** a posizone 4, 50 byte, char, ascending.
018400011115     C                   EVAL      QLGSP      = 4
018500011115     C                   EVAL      QLGSS      = %SIZE(S01Des)
018600011115     C                   EVAL      QLGDT      = Carattere
018700011115     C                   EVAL      QLGSO      = Ascendente
018800011115     C                   EVAL      QLGKL(1)   = QLGSKL
018900011115
019000011114     C                   EVAL      *IN01 = *OFF
019100011114     C                   EVAL      *IN02 = *ON
019200011114     C                   EVAL      *IN03 = *OFF
019300011115
019400011115     ** Al prossimo F11 ordino per provincia e località.
019500011115     C                   EVAL      WrkSort = Gruppo
019600011115
019700011115     ** Ordinamento per provincia e località.
019800011115     C                   WHEN      WrkSort = Gruppo
019900011115
020000011115     ** 2 campi chiave.
020100011115     C                   EVAL      QLGNBRK    = 2
020200011115
020300011115     ** Il gruppo è
020400011115     ** a posizione 3, 1 byte, char, ascending
020500011115     C                   EVAL      QLGSP      = 3
020600011115     C                   EVAL      QLGSS      = %SIZE(S01Grp)
020700011115     C                   EVAL      QLGDT      = Carattere
020800011115     C                   EVAL      QLGSO      = Ascendente
020900011115     C                   EVAL      QLGKL(1)   = QLGSKL
021000011115
021100011115     ** La descrizione è
021200011115     ** a posizione 4, 50 byte, char, ascending
021300011115     C                   EVAL      QLGSP      = 4
021400011115     C                   EVAL      QLGSS      = %SIZE(S01Des)
021500011115     C                   EVAL      QLGDT      = Carattere
021600011115     C                   EVAL      QLGSO      = Ascendente
021700011115     C                   EVAL      QLGKL(2)   = QLGSKL
021800011115
021900011114     C                   EVAL      *IN01 = *OFF
022000011115     C                   EVAL      *IN02 = *ON
022100011114     C                   EVAL      *IN03 = *ON
022200011115
022300011115     ** Al prossimo F11 ordino per descrizione.
022400011114     C                   EVAL      WrkSort = Descrizione
022500011115
022600011114     C                   ENDSL
022700011114
022800011114      * Load other sort parameters.
022900011115     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
023000011115     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
023100011115     C                   EVAL      QLGRT     = 8
023200011115     C                   EVAL      QLGOKL    = 80
023300011115     C                   EVAL      QLGLKE    = 16
023400011115     C                   EVAL      QLGLSS    = 290
023500011114
023600011114      * Initialize Sort I/O API fields.
023700011115     C                   EVAL      QLGRL00  = QLGRL
023800011115     C                   EVAL      QLGRC00  = 1
023900011115     C                   CLEAR                   QUSEI
024000011115     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
024100000000
024200000000      * First step - Initialize the sort routine.
024300000000     C                   CALL      'QLGSORT'
024400011115     C                   PARM                    QLGSCB
024500000000     C                   PARM                    NotUsed
024600000000     C                   PARM                    NotUsed
024700000000     C                   PARM                    SizeList
024800000000     C                   PARM                    ReturnSize
024900011115     C                   PARM                    QUSEC
025000000000
025100000000      * Next step - Write records to I/O routine.
025200011115     C                   EVAL      QLGRT00 = Put
025300000000
025400011115     C                   DO        RrnLast       Rrn1
025500011115
025600011115     C     Rrn1          CHAIN     S01
025700011115
025800011115     ** Solo le righe con Selected = 'Y' sono riordinate,
025900011115     ** quindi per fare un ordinamento di tutte le righe
026000011115     ** metto 'Y' sempre.
026100000000     C                   EVAL      Selected = 'Y'
026200011115
026300011115     C                   CLEAR                   QUSEI
026400011115     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
026500000000     C                   CALL      'QLGSRTIO'
026600011115     C                   PARM                    QLGSCB00
026700000000     C                   PARM                    SflRcd
026800000000     C                   PARM                    NotUsed
026900000000     C                   PARM                    SizeList
027000000000     C                   PARM                    NotUsed
027100011115     C                   PARM                    QUSEC
027200011115
027300000000     C                   ENDDO
027400000000
027500000000      * Next step - Signal end of input, clear subfile for reload.
027600011115     C                   EVAL      QLGRT00 = EndPut
027700011115     C                   CLEAR                   QUSEI
027800011115     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027900000000
028000000000     C                   CALL      'QLGSRTIO'
028100011115     C                   PARM                    QLGSCB00
028200000000     C                   PARM                    SflRcd
028300000000     C                   PARM                    NotUsed
028400000000     C                   PARM                    SizeList
028500000000     C                   PARM                    NotUsed
028600011115     C                   PARM                    QUSEC
028700000000
028800000000     C                   EVAL      *IN30 = *OFF
028900000000     C                   EVAL      *IN31 = *OFF
029000011113     C                   WRITE     C01
029100000000     C                   EVAL      *IN30 = *ON
029200000000
029300000000      * Final step - Write the records back to the subfile.
029400011115     C                   EVAL      QLGRT00 = Get
029500000000
029600011115     C                   DO        RrnLast       Rrn1
029700011115     C                   CLEAR                   QUSEI
029800011115     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029900000000     C                   CALL      'QLGSRTIO'
030000011115     C                   PARM                    QLGSCB00
030100000000     C                   PARM                    NotUsed
030200000000     C                   PARM                    SflRcd
030300011115     C                   PARM                    QLGRL00
030400000000     C                   PARM                    NotUsed
030500011115     C                   PARM                    QUSEC
030600011113     C                   WRITE     S01
030700000000     C                   ENDDO
030800011115
030900000000     C                   EVAL      *IN31 = *ON
031000011115
031100000000     C                   ENDSR
031200011115
031300011115     ** Operazioni iniziali.
031400011115     C     *INZSR        BEGSR
031500011115
031600011115     C     K02Tab00      KLIST
031700011115     C                   KFLD                    TblKut
031800011115     C                   KFLD                    TblCod
031900011115     C                   EVAL      TblKut = 1
032000011115
032100011115     C                   ENDSR
