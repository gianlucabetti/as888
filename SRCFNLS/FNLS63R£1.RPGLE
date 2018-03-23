000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010116      * FNLS63R *-----------------------------------------------------*
000300000731      *             -  PULIZIA BOLLA PARTENZA SINGOLA  -              *
000400000731      *---------------------------------------------------------------*
000500000731      * Il programma cancella la bolla partenza ricevuta come parame_ *
000600000731      * tro e gli archivi collegati. E' a cura del programma chiaman_ *
000700000731      * te il controllo sulla cancellazione.                          *
000800060210      * Se richiamato in modalità "P" (v.fnls01r) effettua solo alcune*
000900060210      * cancellazioni. (Serve per la gestione dell'F16-Annullamento dei
001000060210      * prepagati)                                                    *
001100000731      *---------------------------------------------------------------*
001200000731     FFNBLP01L  UF   E           K DISK
001300000731     F                                     RENAME(FNBLP000:FNBLP1)
001400000731     FFNARB01L  IF   E           K DISK
001500060216     FFiar401L  UF   E           K DISK
001600030108     FFIAR501L  UF   E           K DISK
001700000731     FFIAR601L  UF   E           K DISK
001800000731     FFIAR701L  UF   E           K DISK
001900040604     FFIAR801L  UF   E           K DISK
002000051115     FFiar901L  UF   E           K DISK
002100000731     FFNBLT01L  UF   E           K DISK
002101120828     FFIART01L  UF   E           K DISK
002200051012     FFIARS01L  UF   E           K DISK
002300000731     FFIARBF1C  UF   E           K DISK
002400991104     FFIARBU1L  UF   E           K DISK
002500000731     FFNEVB01L  UF   E           K DISK
002600001130     FFNBLD01L  UF   E           K DISK
002700070404     FFiarg01l  UF   E           K DISK
002800030529     F**!!!FISGN01L  UF   E           K DISK
002900001130     FFISGN05L  UF   E           K DISK
003000030529     F**!!!                                RENAME(FISGN000:FISGN005)
003200000731      *
003300000801     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
003400000731      *
003500000731      * DS I/O DA PROGRAMMA CHIAMANTE
003600000731      *
003700000731     D DSLS63        E DS                  EXTNAME(FNLS63DS)
003800000731      *
003900000731      * DS PER TRUL06R - CARICAMENTO £X
004000000731      *
004100000731     D KPJBA         E DS
004200940929     IFNARBD00      01
004300940929     IFNARBK00      02
004400991104     IFIARBT00      03
004500940929     IFNARBG00      05
004600950607     IFNARBM00      06
004700950607     IFNARBV00      07
004800991104     IFNARBP00      04
004900000731      *
005000000731      *---------------------------------------------------------------*
005100000731      *                                                               *
005200000731      *   O P E R A Z I O N I   I N I Z I A L I                       *
005300000731      *                                                               *
005400000731      *---------------------------------------------------------------*
005500000731      *
005600000731     C     *ENTRY        PLIST
005700000731     C                   PARM                    KPJBA
005800000731     C                   MOVEL     KPJBU         DSLS63
005900000731      *
006000001130      * TIPO LANCIO, I63TLA "C" -           CHIUSO CON LR
006100001130      * TIPO LANCIO, I63TLA "L" - ELABORO E CHIUDO CON LR
006200001130      * TIPO LANCIO, I63TLA " " - ELABORO E CHIUDO IN RETRN
006300001130      *
006400000925    1C     I63TLA        IFNE      'C'
006500000731      *
006600000731      * ACCEDO ALLA BOLLA PARTENZA
006700000731      * .. TROVATA     = 36 OFF
006800000731      * .. NON TROVATA = 36 ON
006900000731      *
007000000731      *          ======================================
007100060210     c                   if        i63mod = 'P'
007200060210     C     KBLP          CHAIN(N)  FNBLP01L                           36
007300060210     c                   else
007400000731     C     KBLP          CHAIN     FNBLP01L                           36
007500060210     c                   endif
007600000731      *          ======================================
007700000731      *
007800000731      *---------------------------------------------------------------*
007900000731      *                                                               *
008000000731      *   F L U S S O   P R I N C I P A L E                           *
008100000731      *                                                               *
008200000731      *---------------------------------------------------------------*
008300000731      *
008400000731      * SE TROVATA CANCELLO
008500000731      * -------------------
008600000731      *
008700000801---  C     *IN36         IFEQ      *OFF
008800000731      *
008900060210     c                   if        i63mod <> 'P'
009000000731      *
009100051011      * C) STORICI E TASSAZIONE DELLA BOLLA
009200000731      *    LI CANCELLO SOLO SE NON C'E' LA BOLLA IN ARRIVO SE E' LOCALE
009300000731      *
009400021104     C                   SETOFF                                       96
009500021104     C     KBLP          SETLL     FNARB01L                               96
009600021104      * 96 OFF --> BOLLA  NON PRESENTE IN ARB
009700051115    1C     *IN96         IFEQ      *OFF
009800051115
009900051115      * A) RECORD CONTRASSEGNO
010000051115      *
010100051115     C     KBLP          delete    Fiar901L                           32
010200000731      *
010300000731      * CANCELLO ANCHE 6: TASSAZIONE SE NON LOCALE
010400000731      *
010500000731     C     KBLP          CHAIN     FIAR601L                           32
010600991104     C     *IN32         DOWEQ     *OFF
010700000731     C                   DELETE    FIAR6000
010800991104     C     KBLP          READE     FIAR601L                               32
010900991104     C                   ENDDO
011000000731      *
011100000731      * CANCELLO ANCHE 7: ESTENSIONE TASSAZIONE
011200000731      *
011300991104     C     KBLP          SETLL     FIAR701L                               32
011400000731     C     *IN32         IFEQ      '1'
011500991104     C     KBLP          READE     FIAR701L                               32
011600000731     C     *IN32         DOWEQ     '0'
011700000731     C                   DELETE    FIAR7000
011800991104     C     KBLP          READE     FIAR701L                               32
011900000731     C                   ENDDO
012000000731     C                   ENDIF
012100000731      *
012200000731      * FIARBF1C:
012300000731      *
012400930906     C                   SETOFF                                       010203
012500950607     C                   SETOFF                                       040506
012600950607     C                   SETOFF                                       07
012700000731      *
012800991104     C     KBLP          SETLL     FIARBF1C                               32
012900000731     C     *IN32         IFEQ      '1'
013000991104     C     KBLP          READE     FIARBF1C                               32
013100000731     C     *IN32         DOWEQ     '0'
013200000731     C   01              DELETE    FNARBD00
013300000731     C   02              DELETE    FNARBK00
013400000731      *
013500000731     C     *IN03         IFEQ      *ON
013600000731      *
013700000731      * SE C'E' LA 3° VARIA CONTROLLO SE CE NE SONO ANCHE IN FNARBU0F
013800000731      *
013900000731     C     ARBSV3        IFNE      ' '
014000000731      *
014100991104     C     KBLP          SETLL     FIARBU00                               30
014200000731     C     *IN30         IFEQ      *ON
014300991104     C     KBLP          READE     FIARBU00                               30
014400000731      *
014500000731     C     *IN30         DOWEQ     *OFF
014600991104     C                   DELETE    FIARBU00
014700991104     C     KBLP          READE     FIARBU00                               30
014800000731     C                   ENDDO
014900000731     C                   ENDIF
015000000731     C                   ENDIF
015100000731      *
015200000731     C                   DELETE    FIARBT00
015300000731     C                   ENDIF
015400000731      *
015500000731     C   05              DELETE    FNARBG00
015600000731     C   06              DELETE    FNARBM00
015700000731     C   07              DELETE    FNARBV00
015800000731     C   04              DELETE    FNARBP00
015900000731      *
016000930906     C                   SETOFF                                       010203
016100950607     C                   SETOFF                                       040506
016200950607     C                   SETOFF                                       07
016300991104     C     KBLP          READE     FIARBF1C                               32
016400000731     C                   ENDDO
016500000731     C                   ENDIF
016600060217     c*
016700060217      * B) ESTENSIONE DESCRIZIONI
016800060217      *
016900060217     C     KBLP          SETLL     Fiar401L                               32
017000060217     C     *IN32         IFEQ      *ON
017100060217     C     KBLP          READE     Fiar401L                               32
017200060217     C     *IN32         DOWEQ     *OFF
017300060217     C                   DELETE    Fiar4000
017400060217     C     KBLP          READE     Fiar401L                               32
017500060217     C                   ENDDO
017600060217     C                   ENDIF
017700030108      *
017800030108      * E) ESTENSIONI TESTATA BOLLA
017900030108      *
018000030108     C     KBLP          setll     FIAR5000                               32
018100030108    4C                   if        *IN32
018200030108     C     KBLP          reade     FIAR5000                               32
018300030108    5C                   dow       not *IN32
018400030108     C                   DELETE    FIAR5000
018500030108     C     KBLP          reade     FIAR5000                               32
018600030108   -5C                   enddo
018700030108   -4C                   endif
018800040604     c**
018900040604     C     KBLP          setll     FIAR8000                               32
019000040604    4C                   if        *IN32
019100040604     C     KBLP          reade     FIAR8000                               32
019200040604    5C                   dow       not *IN32
019300040604     C                   DELETE    FIAR8000
019400040604     C     KBLP          reade     FIAR8000                               32
019500040604   -5C                   enddo
019600040604   -4C                   endif
019700000731      *
019800040831      * F) EVENTI - li cancello solo se bolla mai trasmessa o non trasm
019900040827      *             essa in sede
020000000731      *
020100040827     c
020200040827     c     blpft2        ifeq      *blanks
020300040827     c     blpft2        oreq      'N'
020400991104     C     KBLP          SETLL     FNEVB01L                               32
020500000731     C     *IN32         IFEQ      *ON
020600991104     C     KBLP          READE     FNEVB01L                               32
020700000731     C     *IN32         DOWEQ     *OFF
020800000731     C                   DELETE    FNEVB01L
020900991104     C     KBLP          READE     FNEVB01L                               32
021000000731     C                   ENDDO
021100000731     C                   ENDIF
021200040831     c                   endif
021300051111     c
021400040827     C                   ENDIF
021500060210     c*
021600060210     c                   endif
021700051111      *
021800051111      * D) DETTAGLIO BOLLA ED ESTENSIONI DETTAGLIO
021900051111     c* spostato sotto la SETLL di fnarb, perchè ARS è da cancellare
022000051111     c*  solo se non presente nemmeno la bolla in FNARB
022100051111      *
022200051111     C     KBLP          CHAIN     FNBLT01L                           33
022300051111    1C                   dow       not *in33
022400051111     c
022500060210    2c                   if        not *in96 or i63mod = 'P'
022600051111     c                   EXSR      DELARS
022700051111    2C                   END
022800051111     c
022900051111     C                   DELETE    FNBLT000
023000051111     c
023100051111     C     KBLP          READE     FNBLT01L                               33
023200051111    2C                   ENDDO
023201120828      *
023202120828    1c                   if        not *in96 or i63mod = 'P'
023203120828     C     KBLP          CHAIN     FIART01L                           33
023204120828    2C                   dow       not *in33
023209120828     C                   DELETE    FIART000
023211120828     C     KBLP          READE     FIART01L                               33
023212120828    2C                   ENDDO
023213120828    1C                   ENDIF
023300000731      *
023400030108      * G) CANCELLO FNBLD SE ESISTE
023500001130      *
023600001130     C     KBLP          SETLL     FNBLD000                               30
023700001130     C     *IN30         IFEQ      *ON
023800001130     C     KBLP          READE     FNBLD000                               30
023900001130     C     *IN30         DOWEQ     *OFF
024000001130     C                   DELETE    FNBLD000
024100001130     C     KBLP          READE     FNBLD000                               30
024200001130     C                   ENDDO
024300001130     C                   ENDIF
024400001130      *
024500030108      * H) CANCELLO FISGN SE ESISTE
024600001130      *
024700030529     C**!!!WDLSG1        IFEQ      *BLANKS
024800030529     C**!!!              MOVE      '5'           WSGN
024900030529     C     KBLP          SETLL     FISGN05l                               30
025000001130     C     *IN30         IFEQ      *ON
025100030529     C     KBLP          READE     FISGN05l                               30
025200001130     C     *IN30         DOWEQ     *OFF
025300030529     C**!!!              EXSR      ANNSGN
025400030529     C                   MOVE      '*'           SGNATB
025500030529     C                   CLEAR                   SGNT6A
025600030529     C                   CLEAR                   SGNT6B
025700030529     C                   CLEAR                   SGNT6C
025800030529     C                   CLEAR                   SGNT6D
025900030529     C                   CLEAR                   SGNT6E
026000030529     C                   CLEAR                   SGNT6F
026100080623     c                   eval      sgndatora = %char(%timestamp:*iso0)
026200030529     C                   UPDATE    FISGN000
026300030529     C     KBLP          READE     FISGN05l                               30
026400001130     C                   ENDDO
026500001130     C                   ENDIF
026600030529     C**!!!              ENDIF
026700070405      * I) FIARG
026800070405      *
026900070405    2c                   if        not *in96 or i63mod = 'P'
027000070405     C     KBLP          delete    Fiarg01L                           32
027100070405     c                   endif
027200080109
028500001130      *
028600060210     c                   if        i63mod <> 'P'
028700000731     C                   DELETE    FNBLP1
028800060210     c                   endif
028900000731      *                    ===========
029000000731      *
029100000801      * SE TUTTO OK NON FLAGGO L'ERRORE
029200000801      *
029300000801     C                   CLEAR                   O63ERR
029400000801     C                   CLEAR                   O63MSG
029500000801      *
029600000801---  C                   ELSE
029700000731      *
029800000731      * SE NON HO TROVATO LA BOLLA FLAGGO L'ERRORE
029900000731      * ------------------------------------------
030000000801      *
030100000801     C                   MOVE      '1'           O63ERR
030200001003     C                   MOVEL     MSG(1)        O63MSG
030300000801      *
030400000801---  C                   ENDIF
030500001130    1C                   ENDIF
030600000925     C*
030700000925     C                   MOVEL     DSLS63        KPJBU
030800000925     C*
030900000925     C* CHIUSURA PGM
031000001130     C     I63TLA        IFEQ      ' '
031100000925     C                   RETURN
031200000925     C                   ELSE
031300000925     C                   SETON                                        LR
031400000925     C                   ENDIF
031500001130      *
031600001130      *---------------------------------------------------------------*
031700001130      *                                                               *
031800001130      *   S U B R O U T I N E S                                       *
031900001130      *                                                               *
032000001130      *---------------------------------------------------------------*
032100000731      *
032200001130      *****************************************************************
032300001130      * OPERAZIONI INIZIALI *INZSR                                    *
032400001130      *****************************************************************
032500001130      *
032600001003     C     *INZSR        BEGSR
032700001130      *
032800001130      * MI SALVO LA KPJBU
032900001130      *
033000001003     C                   MOVEL     KPJBU         W256A           256
033100000925      *
033200000925      * KLIST
033300000925      *
033400000925     C     KBLP          KLIST
033500000925     C                   KFLD                    I63AAS
033600000925     C                   KFLD                    I63LNP
033700000925     C                   KFLD                    I63NRS
033800000925     C                   KFLD                    I63NSP
033900000925      *
034000051012     C     KARS          KLIST
034100000925     C                   KFLD                    BLTFLS
034200000925     C                   KFLD                    BLTLNA
034300000925     C                   KFLD                    BLTNRS
034400000925     C                   KFLD                    BLTNSC
034500001228      *
034600030529     C**!!!KSGN          KLIST
034700030529     C**!!!              KFLD                    BLPTFP
034800030529     C**!!!              KFLD                    BLSNOT
034900001130      *
035000001130     C                   MOVEL     W256A         KPJBU
035100030529     C**!!!              MOVE      ' '           WSGN              1
035200001130      *
035300001130     C                   ENDSR
035400001130      *
035500001130      *****************************************************************
035600001130      * ANNULLAMENTO FISGN                                            *
035700001130      *****************************************************************
035800001130      *
035900030529     C**!!!ANNSGN        BEGSR
036000001130      *
036100030529    1C**!!!SGNATB        IFEQ      'A'
036200030529    2C**!!!SGNFL1        IFEQ      *BLANKS
036300030529     C**!!!              MOVE      *ZEROS        WSGPRG            1 0
036400030529   X2C**!!!              ELSE
036500030529     C**!!!              MOVE      SGNFL1        WSGPRG
036600030529    2C**!!!              ENDIF
036700030529    2C**!!!WSGPRG        IFEQ      0
036800030529     C**!!!              MOVE      '*'           SGNATB
036900030529   X2C**!!!              ELSE
037000030529     C**!!!              SUB       1             WSGPRG
037100030529     C**!!!              MOVE      WSGPRG        SGNFL1
037200030529    2C**!!!              ENDIF
037300030529   X1C**!!!              ELSE
037400030529    2C**!!!SGNATB        IFEQ      *BLANKS
037500030529     C**!!!              MOVEL     '*'           SGNATB
037600030529    2C**!!!              ENDIF
037700030529    1C**!!!              ENDIF
037800030529     C**!!!              CLEAR                   SGNT6A
037900030529     C**!!!              CLEAR                   SGNT6B
038000030529     C**!!!              CLEAR                   SGNT6C
038100030529     C**!!!              CLEAR                   SGNT6D
038200030529     C**!!!              CLEAR                   SGNT6E
038300030529     C**!!!              CLEAR                   SGNT6F
038400030529     C**!!!WSGN          IFEQ      '5'
038500030529     C**!!!              UPDATE    FISGN005
038600030529     C**!!!              ELSE
038700030529     C**!!!              UPDATE    FISGN000
038800030529     C**!!!              MOVE      '1'           WDLSG1            1
038900030529     C**!!!              ENDIF
039000001130      *
039100030529     C**!!!              ENDSR
039200051012     c*---------------------------------------------------------------
039300051012     c* Deleto FNARS se non c'e' nemmeno la bolla arrivi
039400051012     c*---------------------------------------------------------------
039500051012     c     DELARS        BEGSR
039600051012     C     KARS          SETLL     FiarS01L
039700051012     C     KARS          READE     FiarS01L                               36
039800051012     c
039900051012    3C     *IN36         DOWEQ     *OFF
040000051012     c
040100051012     C                   DELETE    FiarS000
040200051012     C     KARS          READE     FiarS01L                               36
040300051012    3C                   ENDDO
040400051012     c
040500051012     c                   ENDSR
040600000801**      MSG
040700000801La bolla non è stata trovata
