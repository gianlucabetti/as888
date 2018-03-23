000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300101210     H*------------------------------------------------------------------------*
000400101210
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800110214     Fazdst00f  IF A E             DISK
000900110214     Fazdss00f  IF A E             DISK
001000110214     Fazdse01l  IF A E           K DISK
001100101210     Fwacpmod1l IF   E           K DISK
001200101210     FPRTF198   O    F  198        PRINTER
001300040609
001400040609
001500040609     D*------------------
001600040609     D* DS DI PROCEDURA
001700040609     D*------------------
001800040609     D TRUL33DS      E DS
001900040609     D KPJBA         E DS
002000101210     D*------------------
002100101210     D* VARIABILI D WRK
002200101210     D*------------------
002300101210     D wInt1           s            100    inz
002400101210     D wRiga           s            198    inz
002500101210     D savCOD          s                   like(DSTCOD)
002600970526     C*------------------------------------------------------------------------*
002700970526     C* MAIN LINES
002800970526     C*------------------------------------------------------------------------*
002900101210     C*
003000101210     C                   setoff                                       31
003100101210     C*
003200101210     C* Verifico se lancio in AGGIORNAMENTO (A)
003300101210     C                   if        wOPE = 'A'
003400101210     C                   seton                                        31
003500101210     C                   eval      wInt1 = '*** ELABORAZ. IN AGGIORNAMENTO ***'
003600101210     C                   else
003700101210     C                   eval      wInt1 = '*** ELABORAZ. IN SIMULAZIONE ***'
003800101210     C                   endif
003900101210     C                   except    rigaErr_T
004000101210     C*
004100101210     C*-----------------
004200101210     C* DESTINATARI
004300101210     C*-----------------
004400110214     C     *START        SETLL     azdst00f
004500101210     C                   DOW       not %eof(azdst00f)
004600101210     C                   READ      azdst00f
004700101210     C*
004800101210     C                   eval      OLDL_PRV = DSTPRV
004900101210     C                   eval      OLDL_CAP = DSTCAP
005000101210     C                   eval      OLDL_LOC = DSTLOC
005100101210     C*
005200101210     C* Verifica x PRV + CAP + LOC
005300101210     C     KEYcpmod1_PCL chain     wacpmod1l
005400101210     C                   if        %found(wacpmod1l)
005500101210     C*
005600101210     C* Solo provincia nuove
005700101210     C                   if        NEWL_PRV = 'MB' or
005800101210     C                             NEWL_PRV = 'FM' or
005900101210     C                             NEWL_PRV = 'BT'
006000101210     C*
006100101210     C* Salvo il codice "origine"
006200101210     C                   eval      savCOD = DSTCOD
006300101210     C*
006400101210     C* Stacco 1 nuovo numeratore
006500020422     C                   CLEAR                   TRUL33DS
006600020422     C                   EVAL      I33TLA = 'L'
006700020422     C                   EVAL      I33OPE = *zeros
006800020422     C                   EVAL      I33CNU = 030
006900020422     C                   EVAL      I33NUM = 1
007000020422     C                   MOVEL     TRUL33DS      KPJBU
007100020422     C                   CALL      'TRUL33R'
007200020422     C                   PARM                    KPJBA
007300020422     C                   MOVEL     KPJBU         TRUL33DS
007400040609     C                   IF        O33ERR = *zeros
007500101210     C                   EVAL      DSTCOD = O33NRF
007600101210     C*
007700101210     C* Imposto il nuovo buffer
007800101210     C                   eval      DSTPRV = NEWL_PRV
007900101210     C                   eval      DSTCAP = NEWL_CAP
008000101210     C                   eval      DSTLOC = NEWL_LOC
008100101210     C*
008200101210     C* Infine inserisco nuovo record
008300110214     C   31              WRITE(e)  azdst000
008400110214     C*
008500110214     C                   if        not %error
008600101210     C*
008700101210     C* Duplico anche tutte le relative estenzioni
008800101210     C     KEYdse01_P    setll     azdse01l
008900101210     C                   if        %found(azdse01l)
009000101210     C     KEYdse01_P    reade     azdse01l
009100101210     C                   dow       not %eof(azdse01l)
009200101210     C                   eval      DSECOD = DSTCOD
009300101210     C   31              write     azdse000
009400101210     C     KEYdse01_P    reade     azdse01l
009500101210     C                   enddo
009600101210     C                   endif
009700101210     C*
009800101210     C* Listo operazione
009900101210     C                   eval      wRiga = 'Duplicato DESTINATARIO => ' +
010000101210     C                                     DSTRAG   + ' ' +
010100101210     C                                     OLDL_PRV + ' ' +
010200101210     C                                     OLDL_CAP + ' ' +
010300101210     C                                     OLDL_LOC + ' ' + ' => ' +
010400101210     C                                     DSTPRV   + ' ' +
010500101210     C                                     DSTCAP   + ' ' +
010600101210     C                                     DSTLOC
010700101210     C                   except    rigaErr
010800110214     C*
010900110214     C                   endif
011000101210     C*
011100101210     C                   ENDIF
011200101210     C                   endif
011300101210     C                   endif
011400101210     C*
011500040609     C                   ENDDO
011600101210     C*
011700101210     C*-----------------
011800101210     C* SINONIMI
011900101210     C*-----------------
012000101210     C*
012100101210     C     *START        SETLL     azdss00f
012200101210     C                   DOW       not %eof(azdss00f)
012300101210     C                   READ      azdss00f
012400101210     C*
012500101210     C                   eval      OLDL_PRV = DSSPRV
012600101210     C                   eval      OLDL_CAP = DSSCAP
012700101210     C                   eval      OLDL_LOC = DSSLOC
012800101210     C*
012900101210     C* Verifica x PRV + CAP + LOC
013000101210     C     KEYcpmod1_PCL chain     wacpmod1l
013100101210     C                   if        %found(wacpmod1l)
013200101210     C*
013300101210     C* Solo provincia nuove
013400101210     C                   if        NEWL_PRV = 'MB' or
013500101210     C                             NEWL_PRV = 'FM' or
013600101210     C                             NEWL_PRV = 'BT'
013700101210     C*
013800101210     C* Imposto il nuovo buffer
013900101210     C                   eval      DSSPRV = NEWL_PRV
014000101210     C                   eval      DSSCAP = NEWL_CAP
014100101210     C                   eval      DSSLOC = NEWL_LOC
014200101210     C*
014300101210     C* Infine inserisco nuovo record
014400110214     C   31              WRITE(e)  azdss000
014500110214     C*
014600110214     C                   if        not %error
014700101210     C*
014800101210     C* Listo operazione
014900101210     C                   eval      wRiga = 'Duplicato SINONIMO     => ' +
015000101210     C                                     DSSSIN   + ' ' +
015100101210     C                                     OLDL_PRV + ' ' +
015200101210     C                                     OLDL_CAP + ' ' +
015300101210     C                                     OLDL_LOC + ' ' + ' => ' +
015400101210     C                                     DSSPRV   + ' ' +
015500101210     C                                     DSSCAP   + ' ' +
015600101210     C                                     DSSLOC
015700101210     C                   except    rigaErr
015800110214     C*
015900110214     C                   endif
016000101210     C*
016100101210     C                   endif
016200101210     C                   endif
016300101210     C*
016400101210     C                   ENDDO
016500000407     C*
016600040609     C                   SETON                                        LR
016700101210
016800101210
016900101210      /TITLE Operazioni iniziali.
017000101210     C     *inzsr        BEGSR
017100101210     C*
017200101210     C     *ENTRY        PLIST
017300101210     C                   parm                    wOPE              1
017400101210     C*
017500101210     C* Definizione chiave su WACPMOD1L  - Completa
017600101210     C     KEYcpmod1_PCL KLIST
017700101210     C                   KFLD                    OLDL_PRV
017800101210     C                   KFLD                    OLDL_CAP
017900101210     C                   KFLD                    OLDL_LOC
018000101210     C*
018100101210     C* Definizione chiave su AZDSE01L   - Parziale
018200101210     C     KEYdse01_P    KLIST
018300101210     C                   KFLD                    savCOD
018400101210     C*
018500101210     C                   ENDSR
018600101210     C*------------------------------------------------------------------------*
018700101210     OPRTF198   E            rigaErr_T   1
018800101210     O                       wInt1
018900101210     OPRTF198   E            rigaErr     1
019000101210     O                       wRiga
