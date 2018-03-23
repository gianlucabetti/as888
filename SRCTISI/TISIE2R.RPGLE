000100000000     h*--------------------------------------------------------------------------------------------*
000200000118     h* Reperimento versione cappario DPD
000300060510     h* input  -> data di riferimento (se =      0 prende data corrente)
000400060510     h*                               (se = *all 9 prende ultima versione)
000500060510     h* output -> versione Bartolini
000600060510     h*           versione DPD
000700060510     h*           descrizione
000800000000     h*--------------------------------------------------------------------------------------------*
000900000000     h DECEDIT('0,') DATEDIT(*DMY.)
001000000000     f*--------------------------------------------------------------------------------------------*
001100000000     f* Data base
001200000000     f*--------------------------------------------------------------------------------------------*
001300060510     fdpcve02l  IF   E           K DISK                                         *versioni cappario
001400000000     d*--------------------------------------------------------------------------------------------*
001500000000     d* Data structure
001600000000     d*--------------------------------------------------------------------------------------------*
001700060510     D*------------------
001800060510     D* DS "XSRDA8" - CONTROLLA DATA (8)
001900060510     D*------------------
002000060510     D WLBDA8          DS
002100060510     D  G08DAT                 1      8  0
002200060510     D  G08INV                 9     16  0
002300060510     D  G08ERR                17     17
002400060510     D  G08TGI                18     22  0
002500060510     d*------------------
002600060510     d* DS DI PROCEDURA
002700060510     d*------------------
002800060510     d TISIE2DS      E DS
002900000000     c*--------------------------------------------------------------------------------------------*
003000000000     c* Main lines
003100000000     c*--------------------------------------------------------------------------------------------*
003200060515     C*
003300060515     C* Verifico subito il tipo lancio richiesto
003400060515     C                   if        SIE2TLA = 'C'                                * solo chiusura
003500060515     C                   seton                                        lr
003600060515     C                   endif
003700060515     C*
003800060515     C                   if        SIE2TLA = 'E'                                * solo elaborazione
003900060515     C                   exsr      esegui
004000060515     C                   seton                                        rt
004100060515     C                   endif
004200060515     C*
004300060515     C                   if        SIE2TLA = *blanks                            * elabora e chiude
004400060515     C                   exsr      esegui
004500060515     C                   seton                                        lr
004600060515     C                   endif
004700060515     C*
004800060515     C*------------------------------------------------------------------------*
004900060515     C* ESEGUI - Routine di reperimento PORTO e VARIE fatturate
005000060515     C*------------------------------------------------------------------------*
005100060515     C     ESEGUI        BEGSR
005200060510     c*
005300060510     c* verifico il passaggio parametri => se data riferimento = *all 9 vado su ultima versione
005400060510     c                   IF        sie2dri = 99999999
005500060510     c* legge le versioni
005600060510     c     *hival        SETLL     dpcve02l
005700060510     c                   READP     dpcve02l                               98
005800060510     c                   DOW       NOT *in98
005900060510     c                   IF        cveatb=*blanks                               *non annullata
006000060510     c                   Z-ADD     cvever        sie2ver                        *versione Bartolini
006100060515     c                   Z-ADD     cvedde        sie2dde                        *decorrenza
006200060510     c                   MOVEL     cveverd       sie2verd                       *versione DPD
006300060510     c                   MOVEL     cvedes        sie2des                        *descrizione
006400060510     c                   LEAVE                                                  *fine ciclo
006500060510     c                   ENDIF
006600060510     c                   READP     dpcve02l                               98
006700060510     c                   ENDDO                                                  *fine versioni
006800060510     c*
006900060510     c                   ELSE
007000060510     c*
007100060510     c* verifico il passaggio parametri => se data riferimento = *zeros imposto data corrente
007200060510     c                   IF        sie2dri = *zeros
007300060510     c                   EVAL      sie2dri = datcor
007400060510     c                   ENDIF
007500000000     c*
007600000000     c* legge le versioni
007700060510     c     *hival        SETLL     dpcve02l
007800060510     c                   READP     dpcve02l                               98
007900060510     c                   DOW       NOT *in98
008000060510     c                   IF        cveatb=*blanks                               *non annullata
008100060510     c                   IF        sie2dri >= cvedde AND
008200060510     c                             sie2dri <= cvedsc
008300060510     c                   Z-ADD     cvever        sie2ver                        *versione Bartolini
008400060515     c                   Z-ADD     cvedde        sie2dde                        *decorrenza
008500060510     c                   MOVEL     cveverd       sie2verd                       *versione DPD
008600060510     c                   MOVEL     cvedes        sie2des                        *descrizione
008700000000     c                   LEAVE                                                  *fine ciclo
008800060510     c                   ENDIF
008900060510     c                   ENDIF
009000060510     c                   READP     dpcve02l                               98
009100060510     c                   ENDDO                                                  *fine versioni
009200060510     c                   ENDIF
009300000000     c*
009400060510     c                   SETON                                            lr
009500060515     C*
009600060515     C                   ENDSR
009700000000     c*--------------------------------------------------------------------------------------------*
009800000000     C* *inzsr - operazioni iniziali
009900000000     c*--------------------------------------------------------------------------------------------*
010000000000     C     *inzsr        BEGSR
010100000000     c*
010200000000     c* ricevimento parametri
010300000000     c     *ENTRY        PLIST
010400060510     c                   PARM                    TISIE2DS
010500060510     C*
010600060510     C* CALCOLA LA DATA CORRENTE
010700060510     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
010800060510     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
010900060510     C                   Z-ADD     WNUM8         G08DAT
011000060510     C                   Z-ADD     *ZEROS        G08INV
011100060510     C                   MOVEL     '0'           G08ERR
011200060510     C                   CALL      'XSRDA8'
011300060510     C                   PARM                    WLBDA8
011400060510     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
011500000124     c*
011600000000     c                   ENDSR
