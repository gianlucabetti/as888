000100161214     /*END
000200050705      *------------------------------------------------------------------------*
000300050705      * Pulizia saldi letture macchinone
000400050705      *------------------------------------------------------------------------*
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600050705      *------------------------------------------------------------------------*
000700050705      * DATA BASE
000800050705      *------------------------------------------------------------------------*
000900031112     fFNFVV09L  if   e           k disk
001000031112     fFLTR901L  Uf   e           k disk
001100031112      *
001200080620     Ffimsc00f  UF   E             DISK
001300170210      *
001400170303     fFNMSA00F  Uf   e             disk
001500170303     fFNMSS00F  Uf   e             disk
001600170210      *
001700170210     fFIMSP10F  Uf   e             disk
001800170210     fFIMSP00F  Uf   e             disk
001900050705      *------------------------------------------------------------------------*
002000050705      * SCHIERE
002100050705      *------------------------------------------------------------------------*
002200991230     D* DS ESTERNA PER REPERIMENTO PARAMETRI
002300991230     D*---
002400000406     D PARAM           DS
002500000406     D  DATPUL                 5     12  0
002600020620     D  DATMSC                29     36  0
002700050705     d  DatMSS                37     44  0
002800170210     d  DatMSP0               45     52  0 inz
002900170210     d  DatMSP1               53     60  0 inz
003000000331     D*---
003100000406     D* DS ARCHITETTURA
003200000406     D*---
003300000406     D KPJBA         E DS
003400031112      * - Flag
003500031112     d $OKfva          s              1    inz(*off)
003600031112     d $Delete         s              1    inz(*off)
003700050705      *------------------------------------------------------------------------*
003800050705      * MAIN LINES
003900050705      *------------------------------------------------------------------------*
004000050705     C     *ENTRY        PLIST
004100050705     C                   PARM                    KPJBA
004200050705     C                   MOVEL     KPJBU         PARAM
004300050705     C*
004400020620     C                   IF        DATPUL > *zeros
004500000406     C                   EXSR      CANMSA
004600031112     C                   exsr      CanTR9
004700020620     C                   ENDIF
004800020620      *
004900020620     C                   IF        DATMSC > *zeros
005000000406     C                   EXSR      CANMSC
005100020620     C                   ENDIF
005200050705      *
005300050705     c                   if        DatMSS > *zeros
005400050705     c                   exsr      CanMSS
005500050705     c                   endif
005600170210      *
005700170210     c                   if        DatMSP1 > *zeros
005800170210     c                   exsr      sr_CanMSP1
005900170210     c                   endif
006000170210      *
006100170210     c                   if        DatMSP0 > *zeros
006200170210     c                   exsr      sr_CanMSP0
006300170210     c                   endif
006400000406     C*
006500020620     C                   eval      *inlr = *on
006600050705      *------------------------------------------------------------------------*
006700050705      * CANMSA - OPERAZIONI PULITURA ARCHIVIO FNMSA00F
006800050705      *------------------------------------------------------------------------*
006900020620     C     CANMSA        BEGSR
007000020620     C*
007100020620     C* LEGGE FINO A:
007200020620     C*     - FINE FILE
007300061013      *
007400170303      * - FNMSA00F
007500080407     c                   read      FNMSA000
007600170303do  1c                   DOW       NOT %eof(FNMSA00F)
007700061013      *
007800061013if  2c                   if        MSAdfv <= DatPUL
007900080407     c                   DELETE    FNMSA000
008000061013e   2c                   endif
008100061013      *
008200080407     c                   read      FNMSA000
008300061013e   1c                   ENDDO
008400170303      *
008500020620     C                   ENDSR
008600050705      *------------------------------------------------------------------------*
008700050705      * CANMSC - OPERAZIONI PULITURA ARCHIVIO FNMSC00F
008800050705      *------------------------------------------------------------------------*
008900000406     C     CANMSC        BEGSR
009000000406     C*
009100000406     C* LEGGE FINO A:
009200000406     C*     - FINE FILE
009300080620     C                   READ      fimsc00f                               97
009400000406     C*
009500000406     C     *IN97         DOWEQ     *OFF
009600000406     C*
009700020620     C     mscdtl        IFLE      datmsc
009800080620     C                   delete    fimsc000
009900000406     C                   ENDIF
010000080620     C                   READ      fimsc00f                               97
010100000406     C*
010200000406     C                   ENDDO
010300000406     C*
010400000406     C                   ENDSR
010500050705      *------------------------------------------------------------------------*
010600050705      * CANMSS - OPERAZIONI PULITURA ARCHIVIO FNMSS00F
010700050705      *------------------------------------------------------------------------*
010800000406     C     CANMSS        BEGSR
010900000406     C*
011000000406     C* LEGGE FINO A:
011100000406     C*     - FINE FILE
011200061013      *
011300170303      * - FNMSS00F
011400080407     c                   read      FNMSS000
011500170303do  1c                   DOW       NOT %eof(FNMSS00F)
011600061013      *
011700061013if  2c                   if        MSSdfv <= DatMSS
011800080407     c                   DELETE    FNMSS000
011900061013e   2c                   endif
012000061013      *
012100080407     c                   read      FNMSS000
012200061013e   1c                   ENDDO
012300161214      *
012400000406     C                   ENDSR
012500170210      *------------------------------------------------------------------------*
012600170210      *?CanMSP1 - Operazioni pulitura archivio FIMSP10F                       ?*
012700170210      *------------------------------------------------------------------------*
012800170210     c     sr_CanMSP1    BEGSR
012900170210      *
013000170210      * -?Legge fino a:  FINE file?
013100170210      *
013200170210      * -?FIMSP10F?
013300170210     c                   read      FIMSP100
013400170210do  1c                   DoW       Not %eof(FIMSP10F)
013500170210      *
013600170210if  2c                   if        MSP1dtl <= DatMSP1
013700170210     c                   DELETE    FIMSP100
013800170210e   2c                   endif
013900170210      *
014000170210     c                   read      FIMSP100
014100170210e   1c                   EndDo
014200170210      *
014300170210     c                   ENDSR
014400170210      *------------------------------------------------------------------------*
014500170210      *?CanMSP0 - Operazioni pulitura archivio FIMSP00F                       ?*
014600170210      *------------------------------------------------------------------------*
014700170210     c     sr_CanMSP0    BEGSR
014800170210      *
014900170210      * -?Legge fino a:  FINE file?
015000170210      *
015100170210      * -?FIMSP00F?
015200170210     c                   read      FIMSP000
015300170210do  1c                   DoW       Not %eof(FIMSP00F)
015400170210      *
015500170210if  2c                   if        MSPdtl <= DatMSP0
015600170210     c                   DELETE    FIMSP000
015700170210e   2c                   endif
015800170210      *
015900170210     c                   read      FIMSP000
016000170210e   1c                   EndDo
016100170210      *
016200170210     c                   ENDSR
016300031112      *------------------------------------------------------------------------*
016400031112      * CanTR9 - OPERAZIONI PULITURA ARCHIVIO FLTR900F
016500031112      *------------------------------------------------------------------------*
016600031112     c     CanTR9        BEGSR
016700031112      *
016800031112      * Definizione Key-List
016900031112      * - FNFVV09L
017000031112     c     K03FNFVV      klist
017100031112     c                   kfld                    TR9fgs
017200031112     c                   kfld                    TR9npgA
017300031112     c                   kfld                    FVVnfv
017400031112      * - FLTR901L
017500031112     c     K03FLTR9      klist
017600031112     c                   kfld                    TR9fgs
017700031112     c                   kfld                    TR9npgA
017800031112     c                   kfld                    TR9nfvA
017900031112      *
018000031112      * Posizionamento iniziale sul file FLTR901L
018100031112     c     *loval        setll     FLTR9000
018200031112     c                   read      FLTR9000
018300031112      * Ciclo di elaborazione del file FLTR9
018400031112do  1c                   dow       NOT %eof(FLTR901L)
018500031112      *
018600031112      * - controllo del record in esame
018700031113     c                   exsr      CtrlFLTR9
018800031113      *
018900031113if  2c                   if        $Delete = *on
019000031113      * - cancellazione dei record per F.V. arrivi  (se da cancellare)
019100031112     c                   exsr      DeletFLTR9
019200031113x   2c                   else
019300031113      * - posizionamento sul F.V. arrivi successivo (se da lasciare)
019400031113     c     K03FLTR9      setgt     FLTR9000
019500031113e   2c                   endif
019600031112      *
019700031112      * - lettura del record relativo al F.V. arrivi successivo
019800031112     c                   read      FLTR9000
019900031112e   1c                   enddo
020000031112      *
020100031112     c                   ENDSR
020200031112      *
020300031112      *---------------------------------------------------------------*
020400031112      * Controllo se F.V. arrivi da elaborare                         *
020500031112      *---------------------------------------------------------------*
020600031112     c     CtrlFLTR9     BEGSR
020700031112      *
020800031113     c                   eval      $Delete =  *off
020900031112      *
021000031113      * Controllo dati del file FLTR9
021100031113      *
021200031113      * - F.V. arrivi di default (da lasciare)
021300031112if  1c                   if            TR9npga =  *zeros
021400031112     c                             and TR9nfva =  *zeros
021500031113     c                   goto      NO_Del
021600031113e   1c                   endif
021700031113      *
021800031113      * Controllo dati del file FNFVV
021900031113      *
022000031113     c                   z-add     TR9nfva       FVVnfv
022100031113     c     K03FNFVV      chain     FNFVV000
022200031113      * - F.V. arrivi reperito & non annullato & datato oltre DATPUL
022300031113      *   - chiuso o aperto che sia - (da lasciare)
022400031113if  1c                   if            %found(FNFVV09L)
022500031113     c                             and FVVatb =  *blanks
022600031113     c                             and FVVdfv >  DATpul
022700031113     c***                          and FVVfcf =  *blanks
022800031113     c                   goto      NO_Del
022900031113e   1c                   endif
023000031113      *
023100031113      * F.V. arrivi da cancellare da FLTR9
023200031113     c                   eval      $Delete =  *on
023300031113      *
023400031113     c     NO_Del        tag
023500031112      *
023600031112     c                   ENDSR
023700031112      *
023800031112      *---------------------------------------------------------------*
023900031112      * Cancellazione F.V. arrivi dal file FLTR9                      *
024000031112      *---------------------------------------------------------------*
024100031112     c     DeletFLTR9    BEGSR
024200031112      *
024300031112     c     K03FLTR9      setll     FLTR9000
024400031112      *
024500031112     c                   dou       NOT %found(FLTR901L)
024600031112     c     K03FLTR9      delete    FLTR9000
024700031112     c                   enddo
024800031112      *
024900031112     c                   ENDSR
