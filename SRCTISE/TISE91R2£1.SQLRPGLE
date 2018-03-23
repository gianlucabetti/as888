000100090818     H**************************************************************************
000200090818      *                                                                        *
000300090818      *    Il programma evidenzierà sul file WADELCLP i CLIENTI PERSI          *
000400090818      *                            ossia                                       *
000500090818      *    quelli che NON hanno avuto SPEDIZIONI negli ultimi 4 MESI.          *
000600090818      *                                                                        *
000700090818      *     Al momento il programma verrà lanciato quadrimestralmente.         *
000800090818      *                                                                        *
000900090818     H**************************************************************************
001000090818     H*    Partendo dal WADELPCT precedentemente generato.                     *
001100090818     H*------------------------------------------------------------------------*
001200090818     H*      in base al parametro temporale:    M -> Mensile                   *
001300090818     H*                                         B -> Bimestrale                *
001400090818     H*                                         T -> Trimestrale               *
001500090818     H*                                         Q -> Quadrimestrale            *
001600090818     H*                                         S -> Semestrale                *
001700090824     H*                                         N -> NON FARE                  *
001800090818     H*    Genera un nuovo file WADELCLP identico al WADELPCT con in più       *
001900090818     H*    l'anno mese in cui è stato generato il record cliente da            *
002000090818     H*    segnalare (per NON segnalarlo nuovamente nei successivi 12 mesi)    *
002100090818     H*    e la descrizione del motivo di blocco, se il cliente risultasse     *
002200090818     H*    contabilmente BLOCCATO.                                             *
002300090818     H*------------------------------------------------------------------------*
002400090818     H*      Legge il File WADELPCT solo x i CLIENTI di tipo "B" e "C" e       *
002500090818     H*       con il campo E-mail Resp.Trasporti NON BLANK.                    *
002600090818     H*       (Il FILE è organizzato x CLIENTE UNIFICANTE.)                    *
002700090818     H*      Se il cliente fosse già stato SEGNALATO nell'arco di un anno      *
002800090818     H*      (Attenzione occorre controllare la P.IVA nell'arco dell'anno.)    *
002900090818     H*      NON DEVE essere preso in considerazione.                          *
003000090818     H*      Se il cliente fosse considerato CONTAB.BLOCCATO, deve essere      *
003100090818     H*      preso in considerazione solo se il motivo del BLOCCO è fra        *
003200090818     H*      quelli previsti ALTRIMENTI deve essere ignorato.                  *
003300000000     H*------------------------------------------------------------------------*
003400000000     H DECEDIT('0,') DATEDIT(*DMY.)
003500000000     F*------------------------------------------------------------------------*
003600000000     F* DATA BASE
003700000000     F*------------------------------------------------------------------------*
003800090818     FWADELCLP  o  a E             DISK
003900090818      *
004000081027     FTABEL00F  IF   E           K DISK
004100081027     D*------------------------------------------------------------------------*
004200081027     D* SCHIERE
004300081027     D*------------------------------------------------------------------------*
004400090818     D* CAUSALI BLOCCO CLIENTI
004500090818     D*------------------
004600090818     D CBL             S              3    DIM(50)                              *CODICI
004700090818     D DCBL            S             30    DIM(50)                              *DECODIFICHE
004800081027     D*------------------
004900090818     D spMe24          S             11  0 DIM(24)                              *Nr.Spedizioni
005000090818      *
005100090820     D*------------------
005200090820      * DS per lettura e scrittura
005300090820     D  inpDELPCT    e ds                  extNAME(WADELPCT)
005400090820     D                                     prefix(INP:3)
005500090820      *
005600090820     D  outDELPCT    e ds                  extNAME(WADELPCT)
005700090820     D                                     prefix(OUT:3)
005800090820     D*------------------
005900090818     D da_sollecitare  S              1
006000090819     D savIVA          S                   like(WAPIVA)
006100090818     D*------------------
006200080916      * Record Nr. per agganciare il file di totale come file Fisico
006300090818     D RRN_CLP         s              9  0
006400080916      *
006500000000     D*------------------
006600000000     D* ARCHITETTURA
006700000000     D*------------------
006800000000     D KPJBA         E DS
006900080916      *
007000090818     D*------------------
007100090818     D* DS REPERIMENTO DATI UTENTE
007200090818     D*-------------------
007300090818     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
007400090818     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
007500090818     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
007600090818     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
007700090818     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
007800000000     D*------------------
007900000000     D* PARAMETRI DI LANCIO
008000000000     D*------------------
008100000000     D PARAM           DS
008200000000     D  PARDAT                 1      6  0
008300000000     D  PARAAI                 1      4  0
008400000000     D  PARMMI                 5      6  0
008500000000     D  PARDA2                 7     12  0
008600000000     D  PARAAF                 7     10  0
008700000000     D  PARMMF                11     12  0
008800000000     D  PARDCK                13     13
008900000000     D  PARDCC                14     14
009000000000     D  PARDCF                15     15
009100000000     D  PARFIC                16     18  0
009200000000     D  PARARC                19     21  0
009300000000     D  PARDIC                22     22
009400000000     D  PARDFF                23     23
009500000000     D  PARCOF                24     30  0
009600000000     D  PARDIF                31     31
009700000000     D  PARSTA                32     32
009800000824     D  PARSCE                33     33
009900000000     D  PARTSP                34     34
010000000000     D  PARIOE                35     35
010100000000     D  PARCLV                36     36
010200000000     D  PARDPT                37     42  0
010300000000     D  PARDP2                43     48  0
010400000000     D  PARSPD                49     52  1
010500000000     D  PARPDD                53     56  1
010600000000     D  PARPDA                57     60  1
010700000000     D  PARPOS                61     61
010800000825     D  PARCRE                62     62
010900010618     D  PARCL1                63     63
011000010618     D  PARCL2                64     64
011100010618     D  PARSIN                65     65
011200020404     D  PARCL3                66     66
011300020404     D  PARCL4                67     67
011400020404     D  PARNTW                68     68
011500030923     D  PARDFD                69     72  1
011600030923     D  PARDFA                73     76  1
011700030923     D  PARDFDP               77     80  1
011800030923     D  PARDFAP               81     84  1
011900030923     D  PARFPC                85     85
012000030923     D  PARWPC                86     86
012100030923     D  PARWCP                87     87
012200090818     D  PARWfdpv              88     88
012300090818     D  PARCLPtmp             89     89                                         M/B/T/Q/S
012400080916      *
012500090820     D WrkSqlCmd       S            512
012600000000     C*------------------------------------------------------------------------*
012700000000     C* MAIN LINE
012800000000     C*------------------------------------------------------------------------*
012900080916     C     *ENTRY        PLIST
013000080916     C                   PARM                    KPJBA
013100080916      *
013200080916     C                   MOVEL     KPJBU         PARAM
013300081027     C*
013400090820      *? CONTROLLA se LA STATISTICA deve essere ELABORATA ?
013500090818     c                   clear                   resto             3 0
013600090824      * NON FARE
013700090824     c                   if        PARCLPtmp = 'N'
013800090824     c                   add       1             resto
013900090818      * Mensile
014000090824     c                   elseIf    PARCLPtmp = 'M'
014100090824     c                   z-add     0             resto
014200090818      * Bimestrale
014300090818     c                   elseIf    PARCLPtmp = 'B'
014400090818     c     PARMMF        div       2             risulta           3 0
014500090818     c                   mvr                     resto
014600090818      * Trimestrale
014700090818     c                   elseIf    PARCLPtmp = 'T'
014800090818     c     PARMMF        div       3             risulta           3 0
014900090818     c                   mvr                     resto
015000090818      * Qadrimestrale
015100090818     c                   elseIf    PARCLPtmp = 'Q'
015200090818     c     PARMMF        div       4             risulta           3 0
015300090818     c                   mvr                     resto
015400090818      * Semestrale
015500090818     c                   elseIf    PARCLPtmp = 'S'
015600090818     c     PARMMF        div       6             risulta           3 0
015700090818     c                   mvr                     resto
015800090818     c                   end
015900090820      *? Se il resto è maggiore di Zero NON deve ELABORARE ?
016000090818     c                   if        resto = 0
016100090818     C*
016200090818     C*       carica tabelle Area e Divisione x decodifica
016300090820     c                   exsr      Ope_iniziali
016400090818     C*
016500090818     C*       ed Elabora il file Statistico
016600090820     c                   exsr      Elaborazione
016700081027     C*
016800090820     C*       chiude il cursore SQL
016900090820     c                   exsr      Chiusura_SQL
017000090820     C*
017100090818     c                   end
017200090818      *
017300081027     C                   SETON                                        LR
017400090820     C* =========================================================================
017500090820     C* Operazioni_iniziali
017600090820     C* =========================================================================
017700090820     C     Ope_iniziali  BEGSR
017800090820     C*
017900090820     C* CHIAVE LETTURA TABEL00F -
018000090820     C     KEYTAB        KLIST
018100090820     C                   KFLD                    KTBKUT                         *CODICE UTENTE
018200090820     C                   KFLD                    KTBCOD                         *CODICE TABELLA
018300090820     C*
018400090820     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
018500090820     C     *LIKE         DEFINE    TBLCOD        KTBCOD
018600090820     C*--------------------
018700090820     C* CAUSALI BLOCCO CLIENTI
018800090820     C*--------------------
018900090820     C                   CLEAR                   CBL
019000090820     C                   CLEAR                   DCBL
019100090820     C                   Z-ADD     1             KTBKUT
019200090820     C                   MOVEL     'BI'          KTBCOD
019300090820      *
019400090820     C* BLOCCATI solo .... x questi motivi:
019500090820     C*           002 - "Non spedisce più con Noi"
019600090820     C*           003 - "Ci affida poche spedizioni"
019700090820     C*           004 - "Merce fuori Target"
019800090820     C*           005 - "Non ha accettato l'aumento"
019900090820     C*           006 - "Perso per disservizio"
020000090820      *
020100090820     C                   Z-ADD     *ZEROS        C                 5 0
020200090820     C     KEYTAB        SETLL     TABEL00F
020300090820     C     KEYTAB        READE     TABEL00F                               99
020400090820      *
020500090820DO  1C     *IN99         DOWEQ     *OFF
020600090820      *
020700090820IF  2C     TBLFLG        IFEQ      *BLANKS
020800090820     c                   movel     tblKEY        CodKey            3
020900090820IF  2C                   IF        CodKEY = '002' or
021000090820IF  2C                             CodKEY = '003' or
021100090820IF  2C                             CodKEY = '004' or
021200090820IF  2C                             CodKEY = '005' or
021300090820IF  2C                             CodKEY = '006'
021400090820     C                   ADD       1             C
021500090820     C                   MOVEL     TBLKEY        CBL(C)
021600090820     C                   MOVEL     TBLUNI        DCBL(C)
021700090820E   2C                   ENDIF
021800090820E   2C                   ENDIF
021900090820      *
022000090820     C     KEYTAB        READE     TABEL00F                               99
022100090820E   1C                   ENDDO
022200090820     C*--------------------
022300090820      *  Imposta SQL x la lettura
022400090820     C                   EVAL      WrkSqlCmd =
022500090820     C                              'SELECT * from WADELPCT ORDER BY WAPIVA  '
022600090820     C/EXEC SQL
022700090820     C+ PREPARE S1 FROM :WrkSqlCmd
022800090820     C/END-EXEC
022900090820
023000090820     C/EXEC SQL
023100090820     C+ DECLARE A1 CURSOR FOR S1
023200090820     C/END-EXEC
023300090820
023400090820     C/EXEC SQL
023500090820     C+ OPEN A1
023600090820     C/END-EXEC
023700090820     C*--------------------
023800090820      *
023900090820     C                   ENDSR
024000020116     C* =========================================================================
024100090818     C*  Esegue il controllo delle spedizioni sui 4 mesi
024200080916     C* =========================================================================
024300090820     C     LEGGE_RECORD  BEGSR
024400090820      *
024500090820     c                   move      *off          $inEOF            1
024600090820     C/EXEC SQL
024700090820     C+ FETCH NEXT FROM A1 INTO :inpDELPCT
024800090820     C/END-EXEC
024900090820     C                   SELECT
025000090820      * a fine file
025100090820     C                   WHEN      SqlCod = 100
025200090820     c                   move      *on           $inEOF
025300090820      * x errori
025400090820     C                   WHEN      SqlCod < 0
025500090820     C                   seton                                        H1
025600090820      * Lettura record OK
025700090820     C                   OTHER
025800090820      **
025900090820     C                   ENDSL
026000090820
026100090820     c                   ENDSR
026200090820     C* =========================================================================
026300090820     C*  Esegue il controllo delle spedizioni sui 4 mesi
026400090820     C* =========================================================================
026500090820     C     Elaborazione  BEGSR
026600090820      *
026700090818     C* ciclo completo di lettura dell'Estero da sommare
026800090820     C                   EXSR      LEGGE_RECORD
026900090820      *
027000090818     C* CICLO FINO A FINE FILE DI LAVORO
027100090820DO  1C                   DOW        $inEOF = *OFF
027200090820      *
027300090820      *   pulizie di rottura
027400090819     c                   movel     inpiva        savIVA
027500090820     c                   clear                   conta_PIVA        3 0
027600090820     c                   clear                   record_valido     1
027700090820     c                   clear                   outDELPCT
027800090819     C*
027900090819     C*   a rottura di P.IVA
028000090820DO  1C                   DOW        $inEOF = *OFF   and inpiva = savIVA
028100090820     C*
028200090820      *  Conta quante P.IVA ci sono uguali su codici clienti differenti !!!!
028300090820     c                   eval        conta_PIVA = conta_PIVA + 1
028400090820      *? Non i codici 8888 o 9999 ?
028500090820     c                   move      inpCLI        campo4            4
028600090820     c                   if        campo4 <> '8888' and campo4 <> '9999'
028700090819      *
028800090820      *?  Deve elaborare solo: ?
028900090820      *? i clienti di tipo "C"/"B" ?
029000090818     c                   if        inpCLV = 'C' or inpCLV = 'B'
029100090818     C*
029200090820      *? con indirizzo E-mail ?
029300090818     c                   if        inpEmar <> *blank
029400090818     C*
029500090820      *? ...e NON già segnalati nell'arco 1 Anno. ?
029600090820     c                   exsr      oltre_un_ANNO
029700090820      *
029800090820     c                   IF        passato1anno = 'S'
029900090820      *
030000090820      *? se il cliente fosse bloccato ??? ?
030100090820     c                   exsr      tipo_di_Blocco
030200090818     C*---------
030300090820      *? Non Contabilmente Bloccati ?
030400090820      *? oppure ?
030500090820      *? se Bloccati x i soli motivi Precaricati ?
030600090820     c                   if        da_Scrivere = 'S'
030700090818     C*
030800090820      *? infine CONTROLLA se nei 4 mesi precedenti ?
030900090820      *? NON aveva nemmeno una spedizione. ?
031000090818     c                   exsr      Ctrl_4mesi
031100090818     C*
031200090818     C*  Non trovato il Cliente quindi scrive.
031300090820     c                   If        da_sollecitare ='S' and record_valido= *blank
031400090820     c                   eval      record_valido  = 'S'
031500090820     c                   eval        outDELPCT = inpDELPCT
031600090818     c                   end
031700090820     C*
031800090818     c                   endIF
031900090818     c                   endIF
032000090818     c                   endIF
032100090820     c                   endIF
032200090818E   1C                   ENDif
032300090818     C* FINE ANNO
032400090820     C                   EXSR      LEGGE_RECORD
032500090818E   1C                   ENDDO
032600090820      *
032700090820      *  Totali a rottura di P.IVA
032800090820      *? scrive 1 record a parità di P.IVA ?
032900090820     c                   If          record_valido = 'S'
033000090820     c                   exsr      Write_rec
033100090820     c                   end
033200090819      *
033300090819E   1C                   ENDDO
033400090819      *
033500090818     C                   ENDSR
033600090818     C* =========================================================================
033700090820     C*  Controllo se è passato un anno dall'ultima segnalazione
033800090818     C* =========================================================================
033900090820     C     oltre_un_ANNO BEGSR
034000090818      *
034100090820     C*  Pulisce il Nr.Rel.Record e l'anno/mese
034200090820     c                   reset                   rrn_CLP
034300090820     c                   reset                   AnnoMese          6 0
034400090820     c                   reset                   Passato1anno      1
034500090820     C*
034600090820      *? Legge come una CHAIN...il file dei CLIENTI PERSI    ?
034700090820      *? e se NON trova il record può andare avanti          ?
034800090820     C/EXEC SQL
034900090820     C+ SELECT rrn(WADELCLP), WAPaaaamm
035000090820     C+   INTO :RRN_CLP, :AnnoMese
035100090820     C+   FROM WAdelCLP
035200090820     C+  WHERE
035300090820     C+        WAPiva = :inpIVA
035400090820     C+    AND WAPAAAAMM + 100 >  :parDA2
035500090820     C/END-EXEC
035600090820     c                   IF        SQLcod <> 0
035700090820      *
035800090820      *?   Se ha restituito più righe per stessa P.IVA.....SQLCOD -811    ?
035900090820      * Se l'SQL restituisce + di una riga allora occorre mirare sul Cliente oltre che sulla P.IVA
036000090820      *  Segnalando la presenza della stessa P.IVA su più codici.
036100090820     c                   IF        SQLcod = -811
036200090820     C/EXEC SQL
036300090820     C+ SELECT rrn(WADELCLP), WAPaaaamm
036400090820     C+   INTO :RRN_CLP, :AnnoMese
036500090820     C+   FROM WAdelCLP
036600090820     C+  WHERE
036700090820     C+        WAPiva = :inpIVA
036800090820     C+    AND WAPcli = :inpCLI
036900090820     C+    AND WAPAAAAMM + 100 >  :parDA2
037000090820     C/END-EXEC
037100090820     c                   END
037200090820      *
037300090820     c                   IF        SQLcod <> 0
037400090820     c                   move      'S'           Passato1anno      1
037500090820     c                   END
037600090820      *
037700090820     c                   END
037800090820      *
037900090820     C                   ENDSR
038000090820     C* =========================================================================
038100090820     C*  Controlla se fosse contabilmente bloccato
038200090820     C* =========================================================================
038300090820     C     Tipo_di_bloccoBEGSR
038400090820      *
038500090820     c                   clear                   da_Scrivere       1
038600090820      *--------------------
038700090820      * CAUSALE BLOCCO
038800090820      *--------------------
038900090820     C                   MOVEL     *BLANKS       DESCBL           30
039000090820      *
039100090820     c                   if        inpBLO = *BLANK
039200090820     c                   eval        da_Scrivere = 'S'
039300090820     c                   else
039400090820      *
039500090820     C                   CLEAR                   BS69DS
039600090820     C                   CLEAR                   CLPDS
039700090820     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
039800090820     C                   MOVEL     inpCLI        I69KCP                         *CLIENTE X CNCLP
039900090820     C                   MOVEL     inpCLI        I69KAC                         *CLIENTE X CNACO
040000090820     C                   CALL      'TIBS69R'
040100090820     C                   PARM                    BS69DS
040200090820     C                   PARM                    ACODS
040300090820     C                   PARM                    INDDS
040400090820     C                   PARM                    CLPDS
040500090820     C                   PARM                    CLSDS
040600090820IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
040700090820IF  2C     acoABL        IFEQ      '8'                                          *CLIENTE BLOCCATO
040800090820     C                   Z-ADD     1             C
040900090820     C                   SETOFF                                           99
041000090820     C     clpNAR        LOOKUP    CBL(C)                                 99
041100090820      * rientra fra i casi previsti
041200090820IF  3C     *IN99         IFEQ      *ON
041300090820     C                   MOVEL     DCBL(C)       DESCBL
041400090820     c                   eval        da_Scrivere = 'S'
041500090820E   3C                   ENDIF
041600090820E   2C                   ENDIF
041700090820E   1C                   ENDIF
041800090820E   1C                   end
041900090820      *
042000090820     C                   ENDSR
042100090820     C* =========================================================================
042200090820     C*  Esegue il controllo delle spedizioni sui 4 mesi
042300090820     C* =========================================================================
042400090820     C     Ctrl_4mesi    BEGSR
042500090820      *
042600090818     c                   eval      da_sollecitare = *blank
042700090818     c                   clear                   spMe24
042800090818      *
042900090818     c                   eval      spMe24(01) = inpNS_01AP
043000090818     c                   eval      spMe24(02) = inpNS_02AP
043100090818     c                   eval      spMe24(03) = inpNS_03AP
043200090818     c                   eval      spMe24(04) = inpNS_04AP
043300090818     c                   eval      spMe24(05) = inpNS_05AP
043400090818     c                   eval      spMe24(06) = inpNS_06AP
043500090818     c                   eval      spMe24(07) = inpNS_07AP
043600090818     c                   eval      spMe24(08) = inpNS_08AP
043700090818     c                   eval      spMe24(09) = inpNS_09AP
043800090818     c                   eval      spMe24(10) = inpNS_10AP
043900090818     c                   eval      spMe24(11) = inpNS_11AP
044000090818     c                   eval      spMe24(12) = inpNS_12AP
044100090818     c                   eval      spMe24(13) = inpNS_01AC
044200090818     c                   eval      spMe24(14) = inpNS_02AC
044300090818     c                   eval      spMe24(15) = inpNS_03AC
044400090818     c                   eval      spMe24(16) = inpNS_04AC
044500090818     c                   eval      spMe24(17) = inpNS_05AC
044600090818     c                   eval      spMe24(18) = inpNS_06AC
044700090818     c                   eval      spMe24(19) = inpNS_07AC
044800090818     c                   eval      spMe24(20) = inpNS_08AC
044900090818     c                   eval      spMe24(21) = inpNS_09AC
045000090818     c                   eval      spMe24(22) = inpNS_10AC
045100090818     c                   eval      spMe24(23) = inpNS_11AC
045200090818     c                   eval      spMe24(24) = inpNS_12AC
045300090818      *
045400090818      *  Aggiungo sempre i 12 mesi dell'anno precedente per ottenere l'indice
045500090818      *   della schiera del mese di riferimento da cui contare i 4 mesi.
045600090818     c     12            add       PARMMF        alMese            3 0
045700090818      *
045800090818      *  gli utlimi 4 mesi senza spedizioni:
045900090818     c                   IF        spMe24(alMese)   = 0 and
046000090818     c                             spMe24(alMese-1) = 0 and
046100090818     c                             spMe24(alMese-2) = 0 and
046200090818     c                             spMe24(alMese-3) = 0
046300090818      *
046400090818      *?   SI è da sollecitare e va scritto nel file CLIENTI PERSI.  ?
046500090818     c                   eval      da_sollecitare = 'S'
046600090818     c                   end
046700090818      *
046800080916     C                   ENDSR
046900080916     C* =========================================================================
047000080916     C*  Scrive il record poichè non c'era il cliente
047100080916     C* =========================================================================
047200080916     C     Write_rec     BEGSR
047300080916      *
047400090820     c                   clear                   wadelCL0
047500090820      *
047600090820     c                   eval      WAPAAD     = outAAD
047700090820     c                   eval      WAPMMD     = outMMD
047800090820     c                   eval      WAPAAA     = outAAA
047900090820     c                   eval      WAPMMA     = outMMA
048000090820     c                   eval      WAPIOE     = outIOE
048100090820     c                   eval      WAPDIV     = outDIV
048200090820     c                   eval      WAPDID     = outDID
048300090820     c                   eval      WAPARE     = outARE
048400090820     c                   eval      WAPARD     = outARD
048500090820     c                   eval      WAPFIL     = outFIL
048600090820     c                   eval      WAPFID     = outFID
048700090820     c                   eval      WAPCLV     = outCLV
048800090820     c                   eval      WAPCLI     = outCLI
048900090820     c                   eval      WAPCLD     = outCLD
049000090820     c                   eval      WAPITC     = outITC
049100090820     c                   eval      WAPCLN     = outCLN
049200090820     c                   eval      WAPAGE     = outAGE
049300090820     c                   eval      WAPAGD     = outAGD
049400090820     c                   eval      WAPSAP     = outSAP
049500090820     c                   eval      WAPkgP     = outkgP
049600090820     c                   eval      WAPFAP     = outFAP
049700090820     c                   eval      WAPDAP     = outDAP
049800090820     c                   eval      WAPSAC     = outSAC
049900090820     c                   eval      WAPkgC     = outkgC
050000090820     c                   eval      WAPFAC     = outFAC
050100090820     c                   eval      WAPDAC     = outDAC
050200090820     c                   eval      WAPBLO     = outBLO
050300090820     c                   eval      WAPNS_01AP = outNS_01AP
050400090820     c                   eval      WAPkg_01AP = outkg_01AP
050500090820     c                   eval      WAPRR_01AP = outRR_01AP
050600090820     c                   eval      WAPRP_01AP = outRP_01AP
050700090820     c                   eval      WAPNS_02AP = outNS_02AP
050800090820     c                   eval      WAPkg_02AP = outkg_02AP
050900090820     c                   eval      WAPRR_02AP = outRR_02AP
051000090820     c                   eval      WAPRP_02AP = outRP_02AP
051100090820     c                   eval      WAPNS_03AP = outNS_03AP
051200090820     c                   eval      WAPkg_03AP = outkg_03AP
051300090820     c                   eval      WAPRR_03AP = outRR_03AP
051400090820     c                   eval      WAPRP_03AP = outRP_03AP
051500090820     c                   eval      WAPNS_04AP = outNS_04AP
051600090820     c                   eval      WAPkg_04AP = outkg_04AP
051700090820     c                   eval      WAPRR_04AP = outRR_04AP
051800090820     c                   eval      WAPRP_04AP = outRP_04AP
051900090820     c                   eval      WAPNS_05AP = outNS_05AP
052000090820     c                   eval      WAPkg_05AP = outkg_05AP
052100090820     c                   eval      WAPRR_05AP = outRR_05AP
052200090820     c                   eval      WAPRP_05AP = outRP_05AP
052300090820     c                   eval      WAPNS_06AP = outNS_06AP
052400090820     c                   eval      WAPkg_06AP = outkg_06AP
052500090820     c                   eval      WAPRR_06AP = outRR_06AP
052600090820     c                   eval      WAPRP_06AP = outRP_06AP
052700090820     c                   eval      WAPNS_07AP = outNS_07AP
052800090820     c                   eval      WAPkg_07AP = outkg_07AP
052900090820     c                   eval      WAPRR_07AP = outRR_07AP
053000090820     c                   eval      WAPRP_07AP = outRP_07AP
053100090820     c                   eval      WAPNS_08AP = outNS_08AP
053200090820     c                   eval      WAPkg_08AP = outkg_08AP
053300090820     c                   eval      WAPRR_08AP = outRR_08AP
053400090820     c                   eval      WAPRP_08AP = outRP_08AP
053500090820     c                   eval      WAPNS_09AP = outNS_09AP
053600090820     c                   eval      WAPkg_09AP = outkg_09AP
053700090820     c                   eval      WAPRR_09AP = outRR_09AP
053800090820     c                   eval      WAPRP_09AP = outRP_09AP
053900090820     c                   eval      WAPNS_10AP = outNS_10AP
054000090820     c                   eval      WAPkg_10AP = outkg_10AP
054100090820     c                   eval      WAPRR_10AP = outRR_10AP
054200090820     c                   eval      WAPRP_10AP = outRP_10AP
054300090820     c                   eval      WAPNS_11AP = outNS_11AP
054400090820     c                   eval      WAPkg_11AP = outkg_11AP
054500090820     c                   eval      WAPRR_11AP = outRR_11AP
054600090820     c                   eval      WAPRP_11AP = outRP_11AP
054700090820     c                   eval      WAPNS_12AP = outNS_12AP
054800090820     c                   eval      WAPkg_12AP = outkg_12AP
054900090820     c                   eval      WAPRR_12AP = outRR_12AP
055000090820     c                   eval      WAPRP_12AP = outRP_12AP
055100090820     c                   eval      WAPNS_01AC = outNS_01AC
055200090820     c                   eval      WAPkg_01AC = outkg_01AC
055300090820     c                   eval      WAPRR_01AC = outRR_01AC
055400090820     c                   eval      WAPRP_01AC = outRP_01AC
055500090820     c                   eval      WAPNS_02AC = outNS_02AC
055600090820     c                   eval      WAPkg_02AC = outkg_02AC
055700090820     c                   eval      WAPRR_02AC = outRR_02AC
055800090820     c                   eval      WAPRP_02AC = outRP_02AC
055900090820     c                   eval      WAPNS_03AC = outNS_03AC
056000090820     c                   eval      WAPkg_03AC = outkg_03AC
056100090820     c                   eval      WAPRR_03AC = outRR_03AC
056200090820     c                   eval      WAPRP_03AC = outRP_03AC
056300090820     c                   eval      WAPNS_04AC = outNS_04AC
056400090820     c                   eval      WAPkg_04AC = outkg_04AC
056500090820     c                   eval      WAPRR_04AC = outRR_04AC
056600090820     c                   eval      WAPRP_04AC = outRP_04AC
056700090820     c                   eval      WAPNS_05AC = outNS_05AC
056800090820     c                   eval      WAPkg_05AC = outkg_05AC
056900090820     c                   eval      WAPRR_05AC = outRR_05AC
057000090820     c                   eval      WAPRP_05AC = outRP_05AC
057100090820     c                   eval      WAPNS_06AC = outNS_06AC
057200090820     c                   eval      WAPkg_06AC = outkg_06AC
057300090820     c                   eval      WAPRR_06AC = outRR_06AC
057400090820     c                   eval      WAPRP_06AC = outRP_06AC
057500090820     c                   eval      WAPNS_07AC = outNS_07AC
057600090820     c                   eval      WAPkg_07AC = outkg_07AC
057700090820     c                   eval      WAPRR_07AC = outRR_07AC
057800090820     c                   eval      WAPRP_07AC = outRP_07AC
057900090820     c                   eval      WAPNS_08AC = outNS_08AC
058000090820     c                   eval      WAPkg_08AC = outkg_08AC
058100090820     c                   eval      WAPRR_08AC = outRR_08AC
058200090820     c                   eval      WAPRP_08AC = outRP_08AC
058300090820     c                   eval      WAPNS_09AC = outNS_09AC
058400090820     c                   eval      WAPkg_09AC = outkg_09AC
058500090820     c                   eval      WAPRR_09AC = outRR_09AC
058600090820     c                   eval      WAPRP_09AC = outRP_09AC
058700090820     c                   eval      WAPNS_10AC = outNS_10AC
058800090820     c                   eval      WAPkg_10AC = outkg_10AC
058900090820     c                   eval      WAPRR_10AC = outRR_10AC
059000090820     c                   eval      WAPRP_10AC = outRP_10AC
059100090820     c                   eval      WAPNS_11AC = outNS_11AC
059200090820     c                   eval      WAPkg_11AC = outkg_11AC
059300090820     c                   eval      WAPRR_11AC = outRR_11AC
059400090820     c                   eval      WAPRP_11AC = outRP_11AC
059500090820     c                   eval      WAPNS_12AC = outNS_12AC
059600090820     c                   eval      WAPkg_12AC = outkg_12AC
059700090820     c                   eval      WAPRR_12AC = outRR_12AC
059800090820     c                   eval      WAPRP_12AC = outRP_12AC
059900090820     c                   eval      WAPIND     = outIND
060000090820     c                   eval      WAPCAP     = outCAP
060100090820     c                   eval      WAPLOC     = outLOC
060200090820     c                   eval      WAPPRV     = outPRV
060300090820     c                   eval      WAPNAZ     = outNAZ
060400090820     c                   eval      WAPRESP    = outRESP
060500090820     c                   eval      WAPEMAR    = outEMAR
060600090820     c                   eval      WAPCDF     = outCDF
060700090820     c                   eval      WAPIVA     = outIVA
060800090820     c                   eval      WAPTEL     = outTEL
060900090820     c                   eval      WAPNEW_MES = outNEW_MES
061000081013     C*
061100081013      * ?Campi aggiunti: AGE Unificante e Descrizione
061200090820     c                   eval      WAPaud     = outAUD                          *Descr.Unificante
061300090820     c                   eval      WAPauc     = outAUC                          *Cod.  Unificante
061400080916      *
061500090818     c                   eval      WAPDESBLO  = DESCBL
061600090818     c                   eval      WAPAAAAMM  = parDA2
061700090818      *
061800090819      *  Se la P.IVA è presente su più codici lo deve segnalare
061900090820     c                   if        conta_PIVA > 1
062000090820     c                   eval      WAPnote    = '** Stessa P.IVA x altri codici'
062100090819     c                   end
062200090819      *
062300090818     c                   write     wadelCL0
062400080916      *
062500080916     C                   ENDSR
062600090820     C* =========================================================================
062700090820     C*  Chiusura cursore di lettura
062800090820     C* =========================================================================
062900090820     C     Chiusura_SQL  Begsr
063000090820      *
063100090820     C/EXEC SQL
063200090820     C+ CLOSE A1
063300090820     C/END-EXEC
063400090820      *
063500090820     C                   ENDSR
063600090820     C* =========================================================================
