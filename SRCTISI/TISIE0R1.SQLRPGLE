000100940912     H*------------------------------------------------------------------------*
000200930128     H DECEDIT('0,') DATEDIT(*DMY.)
000300060621     H*------------------------------------------------------------------------*
000400060621     FPRTF198   O    F  300        PRINTER OFLIND(*INOF)
000500060621     FTITA430C  IF   E           K DISK
000600111202     Fazorg01l  IF   E           K DISK
000700060621     D*------------------------------------------------------------------------*
000800111202     D fdpd            s              3  0 dim(199)
000900111202     d
001000060621     D DS_TAS        E DS                  EXTNAME(TITAS00F) INZ
001100060621     D TISIE3DS      E DS                  INZ
001200060621     D DSBL4I        E DS                  INZ
001300111202     D og143         E DS                  INZ
001400111202     D kk                             3  0
001500111202     d wrkgetlista     s           4096    varying
001600940912     C*------------------------------------------------------------------------*
001700080306     C*
001800080306     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001900080306     C
002000080306     C/EXEC SQL
002100080306     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002200080306     C/END-EXEC
002300080306     C*
002400070821     C*
002500070821     C* Ricevimento parametri d lancio
002600070821     C     *ENTRY        PLIST
002700070821     C                   PARM                    In_DataRifAlf     8
002800080325     C                   PARM                    In_StrBolleAlf    8
002900070821     C                   MOVE(P)   In_DataRifAlf In_DataRif        8 0
003000080325     C                   MOVE(P)   In_StrBolleAlfIn_StrBolle       8 0
003100070821     C*
003200111202     c* carico skiera linee DPD
003300111202     c                   clear                   kk
003400111202     c     *loval        setll     azorg01l
003500111202     c                   read      azorg01l
003600111202     c                   dow       not %eof(azorg01l)
003700111202     c                   if        orgfag='F'  or orgfag='A'
003800111202     c                   movel     orgde3        og143
003900111202     c                   if        §ogntw='DPD'
004000111202     c                   add       1             kk
004100111202     c                   movel     orgfil        fdpd(kk)
004200111202     c                   endif
004300111202     c                   endif
004400111202     c
004500111202     c                   read      azorg01l
004600111202     c                   enddo
004700111202     c
004800070821     C* Creazione recordset estratto bolle interessate alla verifica
004900111202     c                   eval      wrkgetlista ='SELECT * FROM TITAS00F WHERE +
005000111202     C                             TASLNA IN ('
005100111202      * ciclo sulla sk dei cod bolla di recupero
005200111202     c                   Do        199           KK                3 0
005300111202     c                   If        FDPD(KK) = 0
005400111202     c                   Leave
005500111202     c                   EndIf
005600111202     c
005700111202     c                   If        KK >1
005800111202     c                   eval      wrkgetlista =
005900111202     c                             %trim(wrkgetlista) + ','
006000111202     c                   EndIf
006100111202     c
006200111202     c                   eval      wrkgetlista =
006300111202     c                             %trim(wrkgetlista) + ' ''' +
006400111202     c                             %EDITC(FDPD(KK):'X') + ''''
006500111202     c                   EndDo
006600111202     c
006700111202     c                   Eval      wrkgetlista =
006800111202     c                             %trim(wrkgetlista) +
006900111202     C                             ') AND TASAAS*10000+TASMGS >=' +
007000111202     c                             %editc(In_StrBolle:'X') +
007100111202     C                             ' AND   TASCCA <> ''2'' AND +
007200111202     C                               TASTBL  NOT IN (''A3'', ''F3'') UNION +
007300111202     C                               SELECT * FROM TITAS10F WHERE TASLNA IN('
007400111202      * ciclo sulla sk dei cod bolla di recupero
007500111202     c                   Do        199           KK                3 0
007600111202     c                   If        FDPD(KK) = 0
007700111202     c                   Leave
007800111202     c                   EndIf
007900111202     c
008000111202     c                   If        KK >1
008100111202     c                   eval      wrkgetlista =
008200111202     c                             %trim(wrkgetlista) + ','
008300111202     c                   EndIf
008400111202     c
008500111202     c                   eval      wrkgetlista =
008600111202     c                             %trim(wrkgetlista) + ' ''' +
008700111202     c                             %EDITC(FDPD(KK):'X') + ''''
008800111202     c                   EndDo
008900111202     c                   Eval      wrkgetlista =
009000111202     c                             %trim(wrkgetlista) +
009100111202     C                             ') AND TASAAS*10000+TASMGS >=' +
009200111202     c                             %editc(In_StrBolle:'X') +
009300111202     C                             ' AND   TASCCA <> ''2'' AND +
009400111202     C                               TASTBL  NOT IN (''A3'', ''F3'')  +
009500111202     C                             ORDER BY TASNZD, TASCAD, TASLOD'
009600070821     C*
009700111202     C/EXEC SQL
009800111202     C+ PREPARE s1 FROM :wrkgetlista
009900111202     C/END-EXEC
010000111202     C
010100111202     C/EXEC SQL
010200111202     C+ DECLARE C1 CURSOR FOR S1
010300111202     C/END-EXEC
010400040304     c*
010500040304     C/EXEC SQL
010600060621     C+ OPEN C1
010700040304     C/END-EXEC
010800060621     C
010900060621     C/EXEC SQL
011000060621     C+ FETCH NEXT FROM C1 INTO :DS_TAS
011100060621     C/END-EXEC
011200060621     C
011300060621     C                   DOW       sqlcod = *zeros
011400060621     C*
011500060621     C                   CLEAR                   TISIE3DS
011600060621     C                   EVAL      ISIE3TLA  = 'E'
011700070821     C                   EVAL      ISIE3DRI  = In_DataRif
011800070821     C                   EVAL      ISIE3DSP  = In_DataRif
011900060621     C                   EVAL      ISIE3NZD  = tasNZD
012000060621     C                   EVAL      ISIE3CAD  = tasCAD
012100060621     C                   EVAL      ISIE3CTY  = tasLOD
012200060621     C*
012300060621     C                   IF        tasPKB > 3
012400060621     C                   EVAL      ISIE3SRV  = 101
012500060621     C                   ELSE
012600060621     C                   EVAL      ISIE3SRV  = 136
012700060621     C                   ENDIF
012800060621     C*
012900060621     C                   EVAL      ISIE3LNP  = tasLNP
013000060621     C                   EVAL      ISIE3EXC  = 'N'
013100060621     C*
013200060621     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD
013300060621     C                   EVAL      ISIE3TLA = 'E'
013400060621     C                   CALL      'TISIE3R'
013500060621     C                   PARM                    TISIE3DS
013600070822     C*
013700060621     C* Se ok verifico che il depot d arrivo calcolato sia = a quello già memorizzato
013800070822     C                   MOVEL     *blanks       wCHANGE           1
013900070822     C                   CLEAR                   DSBL4I
014000060621     C                   EVAL      ta4TRC = 'I'
014100060621     C     KEYta430_C    CHAIN     TITA430C
014200060621     C                   IF        %found(TITA430C)
014300060621     C                   EVAL      DSBL4I = ta4NOT
014400070822     C                   IF        OSIE3DDEP <> §B4IDA
014500070822     C                   MOVEL     '?'           wCHANGE
014600080306     C                   EXCEPT    RIGA
014700060621     C                   ENDIF
014800060621     C                   ENDIF
014900070822     C*
015000060621     C
015100040304     C/EXEC SQL
015200060621     C+ FETCH NEXT FROM C1 INTO :DS_TAS
015300040304     C/END-EXEC
015400060621     C
015500040304     C*
015600060621     C                   ENDDO
015700060621     C
015800040304     C/EXEC SQL
015900060621     C+ CLOSE C1
016000040304     C/END-EXEC
016100060621     C*
016200060621     C* LANCIO IL PROGRAMMA DI REPERIMENTO CAP DPD SOLO X CHIUSURA
016300060621     C                   CLEAR                   TISIE3DS
016400060621     C                   EVAL      ISIE3TLA = 'C'
016500060621     C                   CALL      'TISIE3R'
016600060621     C                   PARM                    TISIE3DS
016700060621     C*
016800060621     C                   SETON                                        LR
016900060621     C*------------------------------------------------------------------------*
017000040304     C     *INZSR        BEGSR
017100060621     C*
017200060621     C* TITA430C - Completa
017300060621     C     KEYta430_C    KLIST
017400060621     C                   KFLD                    tasAAS
017500060621     C                   KFLD                    tasLNP
017600060621     C                   KFLD                    tasNRS
017700060621     C                   KFLD                    tasNSP
017800060621     C                   KFLD                    ta4TRC
017900060621     C*
018000040304     C                   ENDSR
018100060621     C*------------------------------------------------------------------------*
018200060621     OPRTF198   E            RIGA           1
018300070704     O                       wCHANGE             +1
018400060621     O                       TASAAS              +1
018500060621     O                       TASLNP              +1
018600060621     O                       TASNRS              +1
018700060621     O                       TASNSP              +1
018800060621     O                       §B4IDA              +5
018900060621     O                       ISIE3DRI            +5
019000060621     O                       ISIE3NZD            +1
019100060621     O                       ISIE3CAD            +1
019200060621     O                       ISIE3CTY            +1
019300060621     O                       ISIE3SRV            +1
019400060621     O                       ISIE3LNP            +1
019500060621     O                       USIE3CAD            +1
019600060621     O                       OSIE3DDEP           +5
019700060621     O                       OSIE3ERR            +5
019800060621     O                       OSIE3ERRL           +1
019900060621     O                       OSIE3ERRD           +1
020000940912     O*------------------------------------------------------------------------*
