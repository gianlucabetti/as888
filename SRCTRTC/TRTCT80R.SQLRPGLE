000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200981111      *----------------------------------------------------*
000300110418      *   LANCIA PGM TRTC81C    --> Download IFTSTA EDI     *
000400110418      *   x i clienti con EDI tipo invio "EW"               *
000500981111      *----------------------------------------------------*
000600080418     FtiVGD01L  IF   E           K DISK
000700981112      *----------------------------------------------------*
000800981202     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
000900110418     D tabelDS       E DS                  EXTNAME(TABEL00F)
001000110419     d ds3k          e ds
001100981202     D CNCR80        E DS
001200981202     D KPJBA         E DS
001300110418     D WrkSqlCmd       S           1024
001400030926      *----------------------------------------------------*
001500030926      *
001600030926      *----------------------------------------------------*
001700030926     C     *ENTRY        PLIST
001800030926     C                   PARM                    KPJBA
001900110419      *
002000110419     c                   SETON                                        LR
002100080418      *
002200080418      * Definisco chiavi di accesso
002300080418     C     KEY_VGD01     KLIST
002400080418     C                   KFLD                    Tipo_File                      "VC"
002500080418     C                   KFLD                    Elab_Cliente                   "0xxxyyyy"
002600080418     C                   KFLD                    Tipo_Scarico                   "EW"
002700110419      *
002800110419     c                   move      'VC'          Tipo_File
002900110419     c                   move      'EW'          Tipo_Scarico
003000030926      *
003100030926      * Recupero codice AS
003200030926     C                   Z-ADD     1             CODUT             1 0
003300030926     C                   CALL      'X§PARUT'
003400030926     C                   PARM                    UTEDSE
003500030926     C                   MOVEL     REC80         CNCR80
003600030926      *------------------------------------------------------------------
003700110419      *?  Legge tab.3K x Cliente Unificante         ?
003800110419      *?  ELABORO x trasmissione EDI -> solo "EW"   ?
003900030926      *------------------------------------------------------------------
004000110419      *  Legge la tab.3K    x cliente Unificante
004100110418     c* MI COMPONGO L'ISTRUZIONE SQL
004200110419     c                   EVAL      WRKSQLCMD =
004300110419     c                             'select ''0'' concat Cod_cliente as CodUNIF'+
004400110419     c                             ' from ( SELECT DISTINCT                   '+
004500110419     c                             'substr(TBLUNI, 27, 7) as Cod_cliente      '+
004600110419     c                             'FROM tabel00f  WHERE                      '+
004700110419     c                             'tblcod=''3K'' and tblkut = 1 and          '+
004800110419     c                             'substr(TBLUNI, 9, 2)=''EW'' ) as Trsm_EW  '+
004900110419     c                             'ORDER BY CodUNIF                          '
005000110418      *
005100110418     C/EXEC SQL
005200110419     C+ PREPARE S1 FROM :WrkSqlCmd
005300110418     C/END-EXEC
005400110418
005500110418     C/EXEC SQL
005600110418     C+ DECLARE A1 CURSOR FOR S1
005700110418     C/END-EXEC
005800110418
005900110418     C/EXEC SQL
006000110418     C+ OPEN A1
006100110418     C/END-EXEC
006200110418     C                   DOU       SqlCod <> 0
006300110418     C/EXEC SQL
006400110419     C+ FETCH NEXT FROM A1 INTO :Elab_Cliente
006500110418     C/END-EXEC
006600110418     C                   SELECT
006700110418     c*
006800110418     C                   WHEN      SqlCod = 0
006900110419     c*
007000110419     c                   exsr      Lancia_81C
007100110418      **
007200110418      * ? EoF
007300110418     C                   WHEN      SqlCod = 100
007400110418      **
007500110418      * ? Errori
007600110418     C                   WHEN      SqlCod <> 0
007700110418      **
007800110418     C                   ENDSL
007900110418      **
008000110418     C                   ENDDO
008100110418     C/EXEC SQL
008200110418     C+ CLOSE A1
008300110418     C/END-EXEC
008400110419     c                   exsr      Spazza_Tutto
008500110419     c                   return
008600110419     c*-------------------------------------------------------------------
008700110630      *  per creare EDI chiama il TRTCT81C che scrive il flat x download
008800110419     c*-------------------------------------------------------------------
008900110419     c     Lancia_81C    begsr
009000981111      *
009100080421     C     KEY_VGD01     setll     TIvgd01L
009200080418      *
009300110419      * Solo se ci sono dati da trasmettere    ?
009400090218     C                   if        %Equal(TIvgd01L)
009500110419      *
009600110419      * ? il TRTC81C  attiva il Download  ?    Scrive IFTSTA x EDI:
009700110630     C*******            CALL      'TRTC81C'
009800110630     C                   CALL      'TRTCT81C'
009900080422     c                   parm                    Elab_Cliente      8
010000080418     c                   parm                    Tipo_File         2
010100080418     c                   parm                    Tipo_Scarico      2
010200981112      *
010300981216     C                   ENDIF                                                  <-- 4 ---
010400110419      *
010500110419     c                   endsr
010600050304      *------------------------------------------------------------------
010700050304      * Dopo il ciclo su TABEL00F della "3K" esegue un controllo se
010800050304      * fosse rimasto qualche file da trasmettere
010900050304      * (caso mai qualche errore avesse impedito l'invio)
011000050304      *------------------------------------------------------------------
011100110419     c     Spazza_Tutto  begsr
011200110419      *
011300050304     c                   clear                   kpjbu
011400050304     c                   movel     'TCC4'        kcoaz
011500050304     c                   movel     'GAITRA201 '  knsif
011600050304     c                   clear                   kbuff
011700050304     c                   eval      kqebi = 'KEDI'
011800050304     c                   eval      kqieb = 'KEDI'
011900050304     c                   eval      knmus = 'EDPCED'
012000050304     c                   eval      knmtd = 'EDPCED'
012100050304     c                   movel     'QPRINTS   '  kqopr
012200050304      *  x provare la procedura mettevo in Hold il lavoro
012300050304     c                   call      'BCH10'
012400050304     c                   parm                    kpjba
012500110419      *
012600110419     c                   endsr
012700110419     c*-------------------------------------------------------------------
