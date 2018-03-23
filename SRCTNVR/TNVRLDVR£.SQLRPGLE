000100020308      /TITLE Rettifiche conteggi
000200011221
000300930111     H DECEDIT('0,') DATEDIT(*DMY.)
000400011221
001000130625     FFiAPD01L  IF   E           K DISK
001001130625     FFiftt02L  IF   E           K DISK
001400130625     Ftnsb30d   CF   E             WORKSTN
001401130625     d*
002400130625     D tnsb30ds      E DS
002600891030     D KPJBA         E DS
003600021203     D fnlv24ds      E DS
003601130625      * - Parametri x Controllo profilo utenti
003602130625     d TIBS34DS      e ds                  inz
003603130625      * - Ds di riferimento al file esterno AZUTE00F
003604130625     d AZUTEDS       e ds                  extname(AZUTE00F)
003605130625      * - Ds per dati organigramma
003606130625     d DDatiUte      e ds
004300020318     D WLBDAT          DS
004400020318     D  G02DAT                 1      8  0
004500020318     D  G02INV                 9     16  0
004600020318     D  G02ERR                17     17
004700020318     D  G02TGI                18     22  0
004701130625     DDATISO           S               D
006000030128      *
008400921222     C                   EXSR      DEFCAM
008401130625      *
008500130625     C                   DO        *hival
010400130625     C                   EXFMT     SB15T01
010600020318     c* fine
010700020318     C                   IF        *INKC
010800020318     c                   leave
010900020318     C                   ENDIF
011000020318     c* controlli
011100921222     C                   EXSR      CTRD1
011600130625     c   99              iter
011601130625     c* fine
011602130625     C                   IF        *INKF
011603130625     c* se richiesta pulizia
011604130625     c                   if        pulv = 'S'
011605130625     C/EXEC SQL
011606130625     C+ DELETE FROM wftilac0f
011607130625     C/END-EXEC
011608130625     c                   end
011609130625     c*
011610130625     C                   CLEAR                   TNSB30DS
011611130625     c                   EVAL      SB30PDR = FREPDR
011612130625     c                   EVAL      SB30DDC = FREDDC
011613130625     c                   EVAL      SB30REC = V1CREC
011614130625     c                   EVAL      SB30RES = V1CRES
011615130625     c                   EVAL      SB30CSR = V1CCSR
011616130625     C                   EVAL      KPJBU = TNSB30DS
011617130625     C                   CALL      'TNSB31R'
011618130625     C                   PARM                    KPJBA
011619130625     C                   EXSR      PUL01
011620130625     C                   ITER
011621130625     C                   ENDIF
016700130625     C*
016800020318     C                   ENDDO
016900130625     c*
017000130625     C                   seton                                        lr
017100921222     C**************************************************************************
017200921222     C* CONTROLLI VIDEO1
017300020308     C**************************************************************************
017400921223     C     CTRD1         BEGSR                                                  *
017500130625     C                   SETOFF                                       990102
017501130625     C                   SETOFF                                       0304
017600020308     C* Filiale Padroncino
017700130625    1C     pov           IFEQ      0
017800020308     C                   SETON                                        0199
017801130625     c                   leavesr
019800030514    1c                   EndIf
019801130625     c*
020500020308     C     '?'           SCAN      PDRV                                   90
020600030514    3C     *IN90         IFEQ      *ON
020700021203     C                   Z-ADD     pov           d24FIL
020800021203     C                   Z-ADD     0             d24pdr
020900021203     C                   MOVEL     'R'           d24FLG
021000130625     C                   MOVEL     'A'           d24tip
021100021203     C                   MOVEL     *BLANKS       d24rsc
021200021203     C                   MOVEL(p)  fnlv24ds      KPJBU
021300020308     C                   CALL      'FNLV24R'
021400020308     C                   PARM                    KPJBA
021500021203     C                   MOVEL     KPJBU         fnlv24ds
021600030214     c                   move      d24pdr        frepdr
021700030214     c                   move      d24pdr        pdrv
021800021203     c                   movel     d24rsc        pdrdv
021900050308   X3c                   end
022000020308     c                   clear                   pdrdv
022100030514    4C     pdrv          IFne      *blanks
022200130625     C                   MOVEL     POV           FREPDR            7 0
022300020318     C                   MOVE      PDRV          FREPDR
022400021203     C     kapdf         CHAIN     fiapd01L                           02
022500030514    5C     *IN02         IFEQ      *ON
022600020308     C     *IN02         OREQ      *OFF
022700030514     C     APDATB        ANDNE     ' '
022800020308     C                   SETON                                        0299
022801130625     c                   leavesr
022900030514   X5c                   else
023000100223     c                   movel(p)  apdrsf        pdrdv
023100030514   E5C                   ENDIF
023200030514   X4c                   else
023300020308     C                   SETON                                        0299
023301130625     c                   leavesr
023400030514   E4c                   end
023700020308     C* Data distinta
023800020308    1C     ddcv          IFGT      0
023900020318     C                   Z-ADD     ddcv          G02DAT
024000020318     C                   MOVEL     *BLANK        G02ERR
024100020318     C                   CALL      'XSRDA8'
024200020318     C                   PARM                    WLBDAT
024300020318    2C     G02ERR        IFEQ      '1'
024400020318     C                   SETON                                        0399
024401130625     c                   leavesr
024500130625   X2c                   Else
024600020318     C                   move      G02dat        ddcv
024700020318     C                   move      G02INV        datiso
024800130625     c                   move      datiso        freddc            8 0
024900030514    2C                   ENDIF
024901130625   X2c                   Else
024902130625     C                   SETON                                        0399
024903130625     c                   leavesr
025200030514   E1C                   ENDIF
025201130625     c*
030100020320     c* controllo che esista il conteggio
030900020320     c                   movel     'C'           fttfvl
031000020320     c     kftt          chain     fiftt02l
031100130625    5c                   if        not %found(fiftt02l)
031400130625     c                   seton                                        0102
031401130625     C                   SETON                                        0399
031700030514    3c                   end
043100011221     C*
043200921223     C                   ENDSR
093600020308     C**************************************************************************
093700921222     C* INIZIALIZZAZIONE
093800020308     C**************************************************************************
093900921223     C     DEFCAM        BEGSR                                                  *
094300030128      *
094400921223     C     *ENTRY        PLIST                                                  *
094500921223     C                   PARM                    KPJBA                          *
103400021203     C     Kapdf         KLIST
103500021203     C                   KFLD                    apdtip
103600021203     C                   KFLD                    frepdr
103700130625     c                   movel     'A'           apdtip
106700020319     c*
106800020319     C     Kftt          KLIST
106900020319     C                   KFLD                    frepdr
107000020319     C                   KFLD                    fttfvl
107100020319     C                   KFLD                    freddc
107101130625     c                   exsr      DatiJob
107102130625     c                   exsr      pul01
114100030128     C*
114200921223     C                   ENDSR
114300020320     C*---------------------------------------------------------------*
114400020320     C* PULISCO 1 FORMATO --------------------------------------------*
114500020320     C*---------------------------------------------------------------*
114600020320     C     PUL01         BEGSR
114601130625     C                   SETOFF                                       990102
114602130625     C                   SETOFF                                       0304
114700020308     C                   CLEAR                   pov
114800020308     C                   CLEAR                   pdrv
114900020308     C                   CLEAR                   DDCv
115000020308     C                   CLEAR                   pdrdv
115001130625     C                   CLEAR                   pulv
115100130625     C                   Z-ADD     DUTPOU        pov
115101130625     C                   EVAL      V1CRES ='S'
115102130625     C                   EVAL      V1CREC ='S'
115103130625     C                   EVAL      V1CCSR ='S'
116500020308     C                   ENDSR
116501130625      *---------------------------------------------------------------*
116502130625      * Reperimento Dati del job (Utente/Operativi)                   *
116503130625      *---------------------------------------------------------------*
116504130625     c     DatiJob       BEGSR
116505130625      *
116506130625     c     *dtaara       define    §azute        azuteds
116507130625     c     *dtaara       define    §datiute      ddatiute
116508130625      *
116509130625     c                   in(E)     *dtaara
116510130625     c                   IF        %ERROR or RSUT = *blanks
116511130625     c                   clear                   Tibs34Ds
116512130625     c                   call      'TIBS34R'
116513130625     c                   parm                    Tibs34Ds
116514130625     c                   in        *dtaara
116515130625     c                   ENDIF
116516130625      *
116517130625     c                   ENDSR
