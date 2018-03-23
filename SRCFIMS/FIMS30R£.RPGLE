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
001400080407     fFNMSA10F  Uf   e             disk
001500080407     fFNMSS10F  Uf   e             disk
001600161214     fFNMSA00F  Uf   e             disk    rename( FNMSA000 : FNMSA_00 )
001700161214     f                                     prefix( SA0 : 3 )
001800161214     fFNMSS00F  Uf   e             disk    rename( FNMSS000 : FNMSS_00 )
001900161214     f                                     prefix( SS0 : 3 )
002000170210      *
002100170210     fFIMSP10F  Uf   e             disk
002200170210     fFIMSP00F  Uf   e             disk
002300050705      *------------------------------------------------------------------------*
002400050705      * SCHIERE
002500050705      *------------------------------------------------------------------------*
002600991230     D* DS ESTERNA PER REPERIMENTO PARAMETRI
002700991230     D*---
002800000406     D PARAM           DS
002900000406     D  DATPUL                 5     12  0
003000020620     D  DATMSC                29     36  0
003100050705     d  DatMSS                37     44  0
003200170210     d  DatMSP0               45     52  0 inz
003300170210     d  DatMSP1               53     60  0 inz
003400000331     D*---
003500000406     D* DS ARCHITETTURA
003600000406     D*---
003700000406     D KPJBA         E DS
003800031112      * - Flag
003900031112     d $OKfva          s              1    inz(*off)
004000031112     d $Delete         s              1    inz(*off)
004100050705      *------------------------------------------------------------------------*
004200050705      * MAIN LINES
004300050705      *------------------------------------------------------------------------*
004400050705     C     *ENTRY        PLIST
004500050705     C                   PARM                    KPJBA
004600050705     C                   MOVEL     KPJBU         PARAM
004700050705     C*
004800020620     C                   IF        DATPUL > *zeros
004900000406     C                   EXSR      CANMSA
005000031112     C                   exsr      CanTR9
005100020620     C                   ENDIF
005200020620      *
005300020620     C                   IF        DATMSC > *zeros
005400000406     C                   EXSR      CANMSC
005500020620     C                   ENDIF
005600050705      *
005700050705     c                   if        DatMSS > *zeros
005800050705     c                   exsr      CanMSS
005900050705     c                   endif
006000170210      *
006100170210     c                   if        DatMSP1 > *zeros
006200170210     c                   exsr      sr_CanMSP1
006300170210     c                   endif
006400170210      *
006500170210     c                   if        DatMSP0 > *zeros
006600170210     c                   exsr      sr_CanMSP0
006700170210     c                   endif
006800000406     C*
006900020620     C                   eval      *inlr = *on
007000050705      *------------------------------------------------------------------------*
007100050705      * CANMSA - OPERAZIONI PULITURA ARCHIVIO FNMSA00F
007200050705      *------------------------------------------------------------------------*
007300020620     C     CANMSA        BEGSR
007400020620     C*
007500020620     C* LEGGE FINO A:
007600020620     C*     - FINE FILE
007700061013      *
007800161214      * - FNMSA10F
007900080407     c                   read      FNMSA000
008000061013do  1c                   DOW       NOT %eof(FNMSA10F)
008100061013      *
008200061013if  2c                   if        MSAdfv <= DatPUL
008300080407     c                   DELETE    FNMSA000
008400061013e   2c                   endif
008500061013      *
008600080407     c                   read      FNMSA000
008700061013e   1c                   ENDDO
008800161214      *
008900161214      * - FNMSA00F
009000161214     c                   read      FNMSA_00
009100161214do  1c                   DOW       NOT %eof(FNMSA00F)
009200161214      *
009300161214if  2c                   if        SA0dfv <= DatPUL
009400161214     c                   DELETE    FNMSA_00
009500161214e   2c                   endif
009600161214      *
009700161214     c                   read      FNMSA_00
009800161214e   1c                   ENDDO
009900020620     C*
010000020620     C                   ENDSR
010100050705      *------------------------------------------------------------------------*
010200050705      * CANMSC - OPERAZIONI PULITURA ARCHIVIO FNMSC00F
010300050705      *------------------------------------------------------------------------*
010400000406     C     CANMSC        BEGSR
010500000406     C*
010600000406     C* LEGGE FINO A:
010700000406     C*     - FINE FILE
010800080620     C                   READ      fimsc00f                               97
010900000406     C*
011000000406     C     *IN97         DOWEQ     *OFF
011100000406     C*
011200020620     C     mscdtl        IFLE      datmsc
011300080620     C                   delete    fimsc000
011400000406     C                   ENDIF
011500080620     C                   READ      fimsc00f                               97
011600000406     C*
011700000406     C                   ENDDO
011800000406     C*
011900000406     C                   ENDSR
012000050705      *------------------------------------------------------------------------*
012100050705      * CANMSS - OPERAZIONI PULITURA ARCHIVIO FNMSS00F
012200050705      *------------------------------------------------------------------------*
012300000406     C     CANMSS        BEGSR
012400000406     C*
012500000406     C* LEGGE FINO A:
012600000406     C*     - FINE FILE
012700061013      *
012800161214      * - FNMSS10F
012900080407     c                   read      FNMSS000
013000061013do  1c                   DOW       NOT %eof(FNMSS10F)
013100061013      *
013200061013if  2c                   if        MSSdfv <= DatMSS
013300080407     c                   DELETE    FNMSS000
013400061013e   2c                   endif
013500061013      *
013600080407     c                   read      FNMSS000
013700061013e   1c                   ENDDO
013800161214      *
013900161214      * - FNMSS00F
014000161214     c                   read      FNMSS_00
014100161214do  1c                   DOW       NOT %eof(FNMSS00F)
014200161214      *
014300161214if  2c                   if        MSSdfv <= DatMSS
014400161214     c                   DELETE    FNMSS_00
014500161214e   2c                   endif
014600161214      *
014700161214     c                   read      FNMSS_00
014800161214e   1c                   ENDDO
014900000406     C*
015000000406     C                   ENDSR
015100170210      *------------------------------------------------------------------------*
015200170210      *?CanMSP1 - Operazioni pulitura archivio FIMSP10F                       ?*
015300170210      *------------------------------------------------------------------------*
015400170210     c     sr_CanMSP1    BEGSR
015500170210      *
015600170210      * -?Legge fino a:  FINE file?
015700170210      *
015800170210      * -?FIMSP10F?
015900170210     c                   read      FIMSP100
016000170210do  1c                   DoW       Not %eof(FIMSP10F)
016100170210      *
016200170210if  2c                   if        MSP1dtl <= DatMSP1
016300170210     c                   DELETE    FIMSP100
016400170210e   2c                   endif
016500170210      *
016600170210     c                   read      FIMSP100
016700170210e   1c                   EndDo
016800170210      *
016900170210     c                   ENDSR
017000170210      *------------------------------------------------------------------------*
017100170210      *?CanMSP0 - Operazioni pulitura archivio FIMSP00F                       ?*
017200170210      *------------------------------------------------------------------------*
017300170210     c     sr_CanMSP0    BEGSR
017400170210      *
017500170210      * -?Legge fino a:  FINE file?
017600170210      *
017700170210      * -?FIMSP00F?
017800170210     c                   read      FIMSP000
017900170210do  1c                   DoW       Not %eof(FIMSP00F)
018000170210      *
018100170210if  2c                   if        MSPdtl <= DatMSP0
018200170210     c                   DELETE    FIMSP000
018300170210e   2c                   endif
018400170210      *
018500170210     c                   read      FIMSP000
018600170210e   1c                   EndDo
018700170210      *
018800170210     c                   ENDSR
018900031112      *------------------------------------------------------------------------*
019000031112      * CanTR9 - OPERAZIONI PULITURA ARCHIVIO FLTR900F
019100031112      *------------------------------------------------------------------------*
019200031112     c     CanTR9        BEGSR
019300031112      *
019400031112      * Definizione Key-List
019500031112      * - FNFVV09L
019600031112     c     K03FNFVV      klist
019700031112     c                   kfld                    TR9fgs
019800031112     c                   kfld                    TR9npgA
019900031112     c                   kfld                    FVVnfv
020000031112      * - FLTR901L
020100031112     c     K03FLTR9      klist
020200031112     c                   kfld                    TR9fgs
020300031112     c                   kfld                    TR9npgA
020400031112     c                   kfld                    TR9nfvA
020500031112      *
020600031112      * Posizionamento iniziale sul file FLTR901L
020700031112     c     *loval        setll     FLTR9000
020800031112     c                   read      FLTR9000
020900031112      * Ciclo di elaborazione del file FLTR9
021000031112do  1c                   dow       NOT %eof(FLTR901L)
021100031112      *
021200031112      * - controllo del record in esame
021300031113     c                   exsr      CtrlFLTR9
021400031113      *
021500031113if  2c                   if        $Delete = *on
021600031113      * - cancellazione dei record per F.V. arrivi  (se da cancellare)
021700031112     c                   exsr      DeletFLTR9
021800031113x   2c                   else
021900031113      * - posizionamento sul F.V. arrivi successivo (se da lasciare)
022000031113     c     K03FLTR9      setgt     FLTR9000
022100031113e   2c                   endif
022200031112      *
022300031112      * - lettura del record relativo al F.V. arrivi successivo
022400031112     c                   read      FLTR9000
022500031112e   1c                   enddo
022600031112      *
022700031112     c                   ENDSR
022800031112      *
022900031112      *---------------------------------------------------------------*
023000031112      * Controllo se F.V. arrivi da elaborare                         *
023100031112      *---------------------------------------------------------------*
023200031112     c     CtrlFLTR9     BEGSR
023300031112      *
023400031113     c                   eval      $Delete =  *off
023500031112      *
023600031113      * Controllo dati del file FLTR9
023700031113      *
023800031113      * - F.V. arrivi di default (da lasciare)
023900031112if  1c                   if            TR9npga =  *zeros
024000031112     c                             and TR9nfva =  *zeros
024100031113     c                   goto      NO_Del
024200031113e   1c                   endif
024300031113      *
024400031113      * Controllo dati del file FNFVV
024500031113      *
024600031113     c                   z-add     TR9nfva       FVVnfv
024700031113     c     K03FNFVV      chain     FNFVV000
024800031113      * - F.V. arrivi reperito & non annullato & datato oltre DATPUL
024900031113      *   - chiuso o aperto che sia - (da lasciare)
025000031113if  1c                   if            %found(FNFVV09L)
025100031113     c                             and FVVatb =  *blanks
025200031113     c                             and FVVdfv >  DATpul
025300031113     c***                          and FVVfcf =  *blanks
025400031113     c                   goto      NO_Del
025500031113e   1c                   endif
025600031113      *
025700031113      * F.V. arrivi da cancellare da FLTR9
025800031113     c                   eval      $Delete =  *on
025900031113      *
026000031113     c     NO_Del        tag
026100031112      *
026200031112     c                   ENDSR
026300031112      *
026400031112      *---------------------------------------------------------------*
026500031112      * Cancellazione F.V. arrivi dal file FLTR9                      *
026600031112      *---------------------------------------------------------------*
026700031112     c     DeletFLTR9    BEGSR
026800031112      *
026900031112     c     K03FLTR9      setll     FLTR9000
027000031112      *
027100031112     c                   dou       NOT %found(FLTR901L)
027200031112     c     K03FLTR9      delete    FLTR9000
027300031112     c                   enddo
027400031112      *
027500031112     c                   ENDSR
