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
002200120828     FFIART01L  UF   E           K DISK
002300051012     FFIARS01L  UF   E           K DISK
002400131014     FFIARBF2C  UF   E           K DISK
002500991104     FFIARBU1L  UF   E           K DISK
002600000731     FFNEVB01L  UF   E           K DISK
002601131031     FFNEVB22L  UF   E           K DISK
002700001130     FFNBLD01L  UF   E           K DISK
002800070404     FFiarg01l  UF   E           K DISK
002900030529     F**!!!FISGN01L  UF   E           K DISK
003000001130     FFISGN05L  UF   E           K DISK
003100030529     F**!!!                                RENAME(FISGN000:FISGN005)
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
004900131014     IFNARBN00      08
005000000731      *
005100000731      *---------------------------------------------------------------*
005200000731      *                                                               *
005300000731      *   O P E R A Z I O N I   I N I Z I A L I                       *
005400000731      *                                                               *
005500000731      *---------------------------------------------------------------*
005600000731      *
005700000731     C     *ENTRY        PLIST
005800000731     C                   PARM                    KPJBA
005900000731     C                   MOVEL     KPJBU         DSLS63
006000000731      *
006100001130      * TIPO LANCIO, I63TLA "C" -           CHIUSO CON LR
006200001130      * TIPO LANCIO, I63TLA "L" - ELABORO E CHIUDO CON LR
006300001130      * TIPO LANCIO, I63TLA " " - ELABORO E CHIUDO IN RETRN
006400001130      *
006500000925    1C     I63TLA        IFNE      'C'
006600000731      *
006700000731      * ACCEDO ALLA BOLLA PARTENZA
006800000731      * .. TROVATA     = 36 OFF
006900000731      * .. NON TROVATA = 36 ON
007000000731      *
007100000731      *          ======================================
007200060210     c                   if        i63mod = 'P'
007300060210     C     KBLP          CHAIN(N)  FNBLP01L                           36
007400060210     c                   else
007500000731     C     KBLP          CHAIN     FNBLP01L                           36
007600060210     c                   endif
007700000731      *          ======================================
007800000731      *
007900000731      *---------------------------------------------------------------*
008000000731      *                                                               *
008100000731      *   F L U S S O   P R I N C I P A L E                           *
008200000731      *                                                               *
008300000731      *---------------------------------------------------------------*
008400000731      *
008500000731      * SE TROVATA CANCELLO
008600000731      * -------------------
008700000731      *
008800000801---  C     *IN36         IFEQ      *OFF
008900000731      *
009000060210     c                   if        i63mod <> 'P'
009100000731      *
009200051011      * C) STORICI E TASSAZIONE DELLA BOLLA
009300000731      *    LI CANCELLO SOLO SE NON C'E' LA BOLLA IN ARRIVO SE E' LOCALE
009400000731      *
009500021104     C                   SETOFF                                       96
009600021104     C     KBLP          SETLL     FNARB01L                               96
009700021104      * 96 OFF --> BOLLA  NON PRESENTE IN ARB
009800051115    1C     *IN96         IFEQ      *OFF
009900051115
010000051115      * A) RECORD CONTRASSEGNO
010100051115      *
010200051115     C     KBLP          delete    Fiar901L                           32
010300000731      *
010400000731      * CANCELLO ANCHE 6: TASSAZIONE SE NON LOCALE
010500000731      *
010600000731     C     KBLP          CHAIN     FIAR601L                           32
010700991104     C     *IN32         DOWEQ     *OFF
010800000731     C                   DELETE    FIAR6000
010900991104     C     KBLP          READE     FIAR601L                               32
011000991104     C                   ENDDO
011100000731      *
011200000731      * CANCELLO ANCHE 7: ESTENSIONE TASSAZIONE
011300000731      *
011400991104     C     KBLP          SETLL     FIAR701L                               32
011500000731     C     *IN32         IFEQ      '1'
011600991104     C     KBLP          READE     FIAR701L                               32
011700000731     C     *IN32         DOWEQ     '0'
011800000731     C                   DELETE    FIAR7000
011900991104     C     KBLP          READE     FIAR701L                               32
012000000731     C                   ENDDO
012100000731     C                   ENDIF
012200000731      *
012300131014      * FIARBF2C:
012400000731      *
012500930906     C                   SETOFF                                       010203
012600950607     C                   SETOFF                                       040506
012700131014     C                   SETOFF                                       0708
012800000731      *
012900131014     C     KBLP          SETLL     FIARBF2C                               32
013000000731     C     *IN32         IFEQ      '1'
013100131014     C     KBLP          READE     FIARBF2C                               32
013200000731     C     *IN32         DOWEQ     '0'
013300000731     C   01              DELETE    FNARBD00
013400000731     C   02              DELETE    FNARBK00
013500000731      *
013600000731     C     *IN03         IFEQ      *ON
013700000731      *
013800000731      * SE C'E' LA 3° VARIA CONTROLLO SE CE NE SONO ANCHE IN FNARBU0F
013900000731      *
014000000731     C     ARBSV3        IFNE      ' '
014100000731      *
014200991104     C     KBLP          SETLL     FIARBU00                               30
014300000731     C     *IN30         IFEQ      *ON
014400991104     C     KBLP          READE     FIARBU00                               30
014500000731      *
014600000731     C     *IN30         DOWEQ     *OFF
014700991104     C                   DELETE    FIARBU00
014800991104     C     KBLP          READE     FIARBU00                               30
014900000731     C                   ENDDO
015000000731     C                   ENDIF
015100000731     C                   ENDIF
015200000731      *
015300000731     C                   DELETE    FIARBT00
015400000731     C                   ENDIF
015500000731      *
015600000731     C   05              DELETE    FNARBG00
015700000731     C   06              DELETE    FNARBM00
015800000731     C   07              DELETE    FNARBV00
015900000731     C   04              DELETE    FNARBP00
016000131014     C   08              DELETE    FNARBN00
016100000731      *
016200930906     C                   SETOFF                                       010203
016300950607     C                   SETOFF                                       040506
016400131014     C                   SETOFF                                       0708
016500131014     C     KBLP          READE     FIARBF2C                               32
016600000731     C                   ENDDO
016700000731     C                   ENDIF
016800060217     c*
016900060217      * B) ESTENSIONE DESCRIZIONI
017000060217      *
017100060217     C     KBLP          SETLL     Fiar401L                               32
017200060217     C     *IN32         IFEQ      *ON
017300060217     C     KBLP          READE     Fiar401L                               32
017400060217     C     *IN32         DOWEQ     *OFF
017500060217     C                   DELETE    Fiar4000
017600060217     C     KBLP          READE     Fiar401L                               32
017700060217     C                   ENDDO
017800060217     C                   ENDIF
017900030108      *
018000030108      * E) ESTENSIONI TESTATA BOLLA
018100030108      *
018200030108     C     KBLP          setll     FIAR5000                               32
018300030108    4C                   if        *IN32
018400030108     C     KBLP          reade     FIAR5000                               32
018500030108    5C                   dow       not *IN32
018600030108     C                   DELETE    FIAR5000
018700030108     C     KBLP          reade     FIAR5000                               32
018800030108   -5C                   enddo
018900030108   -4C                   endif
019000040604     c**
019100040604     C     KBLP          setll     FIAR8000                               32
019200040604    4C                   if        *IN32
019300040604     C     KBLP          reade     FIAR8000                               32
019400040604    5C                   dow       not *IN32
019500040604     C                   DELETE    FIAR8000
019600040604     C     KBLP          reade     FIAR8000                               32
019700040604   -5C                   enddo
019800040604   -4C                   endif
019900000731      *
020000040831      * F) EVENTI - li cancello solo se bolla mai trasmessa o non trasm
020100040827      *             essa in sede
020200000731      *
020300040827     c
020400040827     c     blpft2        ifeq      *blanks
020500040827     c     blpft2        oreq      'N'
020600991104     C     KBLP          SETLL     FNEVB01L                               32
020700000731     C     *IN32         IFEQ      *ON
020800991104     C     KBLP          READE     FNEVB01L                               32
020900000731     C     *IN32         DOWEQ     *OFF
021000000731     C                   DELETE    FNEVB01L
021001131031     C     Kblp2f        chain     FNEVB22L
021002131031     c                   if        %found(fnevb22l)
021003131031     c                   delete    fnevb22l
021004131031     c                   endif
021100991104     C     KBLP          READE     FNEVB01L                               32
021200000731     C                   ENDDO
021300000731     C                   ENDIF
021400040831     c                   endif
021500051111     c
021600040827     C                   ENDIF
021700060210     c*
021800060210     c                   endif
021900051111      *
022000051111      * D) DETTAGLIO BOLLA ED ESTENSIONI DETTAGLIO
022100051111     c* spostato sotto la SETLL di fnarb, perchè ARS è da cancellare
022200051111     c*  solo se non presente nemmeno la bolla in FNARB
022300051111      *
022400051111     C     KBLP          CHAIN     FNBLT01L                           33
022500051111    1C                   dow       not *in33
022600051111     c
022700060210    2c                   if        not *in96 or i63mod = 'P'
022800051111     c                   EXSR      DELARS
022900051111    2C                   END
023000051111     c
023100051111     C                   DELETE    FNBLT000
023200051111     c
023300051111     C     KBLP          READE     FNBLT01L                               33
023400051111    2C                   ENDDO
023500120828      *
023600120828    1c                   if        not *in96 or i63mod = 'P'
023700120828     C     KBLP          CHAIN     FIART01L                           33
023800120828    2C                   dow       not *in33
023900120828     C                   DELETE    FIART000
024000120828     C     KBLP          READE     FIART01L                               33
024100120828    2C                   ENDDO
024200120828    1C                   ENDIF
024300000731      *
024400030108      * G) CANCELLO FNBLD SE ESISTE
024500001130      *
024600001130     C     KBLP          SETLL     FNBLD000                               30
024700001130     C     *IN30         IFEQ      *ON
024800001130     C     KBLP          READE     FNBLD000                               30
024900001130     C     *IN30         DOWEQ     *OFF
025000001130     C                   DELETE    FNBLD000
025100001130     C     KBLP          READE     FNBLD000                               30
025200001130     C                   ENDDO
025300001130     C                   ENDIF
025400001130      *
025500030108      * H) CANCELLO FISGN SE ESISTE
025600001130      *
025700030529     C**!!!WDLSG1        IFEQ      *BLANKS
025800030529     C**!!!              MOVE      '5'           WSGN
025900030529     C     KBLP          SETLL     FISGN05l                               30
026000001130     C     *IN30         IFEQ      *ON
026100030529     C     KBLP          READE     FISGN05l                               30
026200001130     C     *IN30         DOWEQ     *OFF
026300030529     C**!!!              EXSR      ANNSGN
026400030529     C                   MOVE      '*'           SGNATB
026500030529     C                   CLEAR                   SGNT6A
026600030529     C                   CLEAR                   SGNT6B
026700030529     C                   CLEAR                   SGNT6C
026800030529     C                   CLEAR                   SGNT6D
026900030529     C                   CLEAR                   SGNT6E
027000030529     C                   CLEAR                   SGNT6F
027100080623     c                   eval      sgndatora = %char(%timestamp:*iso0)
027200030529     C                   UPDATE    FISGN000
027300030529     C     KBLP          READE     FISGN05l                               30
027400001130     C                   ENDDO
027500001130     C                   ENDIF
027600030529     C**!!!              ENDIF
027700070405      * I) FIARG
027800070405      *
027900070405    2c                   if        not *in96 or i63mod = 'P'
028000070405     C     KBLP          delete    Fiarg01L                           32
028100070405     c                   endif
028200080109
028300001130      *
028400060210     c                   if        i63mod <> 'P'
028500000731     C                   DELETE    FNBLP1
028600060210     c                   endif
028700000731      *                    ===========
028800000731      *
028900000801      * SE TUTTO OK NON FLAGGO L'ERRORE
029000000801      *
029100000801     C                   CLEAR                   O63ERR
029200000801     C                   CLEAR                   O63MSG
029300000801      *
029400000801---  C                   ELSE
029500000731      *
029600000731      * SE NON HO TROVATO LA BOLLA FLAGGO L'ERRORE
029700000731      * ------------------------------------------
029800000801      *
029900000801     C                   MOVE      '1'           O63ERR
030000001003     C                   MOVEL     MSG(1)        O63MSG
030100000801      *
030200000801---  C                   ENDIF
030300001130    1C                   ENDIF
030400000925     C*
030500000925     C                   MOVEL     DSLS63        KPJBU
030600000925     C*
030700000925     C* CHIUSURA PGM
030800001130     C     I63TLA        IFEQ      ' '
030900000925     C                   RETURN
031000000925     C                   ELSE
031100000925     C                   SETON                                        LR
031200000925     C                   ENDIF
031300001130      *
031400001130      *---------------------------------------------------------------*
031500001130      *                                                               *
031600001130      *   S U B R O U T I N E S                                       *
031700001130      *                                                               *
031800001130      *---------------------------------------------------------------*
031900000731      *
032000001130      *****************************************************************
032100001130      * OPERAZIONI INIZIALI *INZSR                                    *
032200001130      *****************************************************************
032300001130      *
032400001003     C     *INZSR        BEGSR
032500001130      *
032600001130      * MI SALVO LA KPJBU
032700001130      *
032800001003     C                   MOVEL     KPJBU         W256A           256
032900000925      *
033000000925      * KLIST
033100000925      *
033200000925     C     KBLP          KLIST
033300000925     C                   KFLD                    I63AAS
033400000925     C                   KFLD                    I63LNP
033500000925     C                   KFLD                    I63NRS
033600000925     C                   KFLD                    I63NSP
033601131031     C     Kblp2f        KLIST
033602131031     C                   KFLD                    EVBAAS
033603131031     C                   KFLD                    EVBLNP
033604131031     C                   KFLD                    EVBNRS
033605131031     C                   KFLD                    EVBNSP
033606131031     C                   KFLD                    EVBDTV
033607131031     C                   KFLD                    EVBORV
033608131031     C                   KFLD                    EVBCEV
033700000925      *
033800051012     C     KARS          KLIST
033900000925     C                   KFLD                    BLTFLS
034000000925     C                   KFLD                    BLTLNA
034100000925     C                   KFLD                    BLTNRS
034200000925     C                   KFLD                    BLTNSC
034300001228      *
034400030529     C**!!!KSGN          KLIST
034500030529     C**!!!              KFLD                    BLPTFP
034600030529     C**!!!              KFLD                    BLSNOT
034700001130      *
034800001130     C                   MOVEL     W256A         KPJBU
034900030529     C**!!!              MOVE      ' '           WSGN              1
035000001130      *
035100001130     C                   ENDSR
035200001130      *
035300001130      *****************************************************************
035400001130      * ANNULLAMENTO FISGN                                            *
035500001130      *****************************************************************
035600001130      *
035700030529     C**!!!ANNSGN        BEGSR
035800001130      *
035900030529    1C**!!!SGNATB        IFEQ      'A'
036000030529    2C**!!!SGNFL1        IFEQ      *BLANKS
036100030529     C**!!!              MOVE      *ZEROS        WSGPRG            1 0
036200030529   X2C**!!!              ELSE
036300030529     C**!!!              MOVE      SGNFL1        WSGPRG
036400030529    2C**!!!              ENDIF
036500030529    2C**!!!WSGPRG        IFEQ      0
036600030529     C**!!!              MOVE      '*'           SGNATB
036700030529   X2C**!!!              ELSE
036800030529     C**!!!              SUB       1             WSGPRG
036900030529     C**!!!              MOVE      WSGPRG        SGNFL1
037000030529    2C**!!!              ENDIF
037100030529   X1C**!!!              ELSE
037200030529    2C**!!!SGNATB        IFEQ      *BLANKS
037300030529     C**!!!              MOVEL     '*'           SGNATB
037400030529    2C**!!!              ENDIF
037500030529    1C**!!!              ENDIF
037600030529     C**!!!              CLEAR                   SGNT6A
037700030529     C**!!!              CLEAR                   SGNT6B
037800030529     C**!!!              CLEAR                   SGNT6C
037900030529     C**!!!              CLEAR                   SGNT6D
038000030529     C**!!!              CLEAR                   SGNT6E
038100030529     C**!!!              CLEAR                   SGNT6F
038200030529     C**!!!WSGN          IFEQ      '5'
038300030529     C**!!!              UPDATE    FISGN005
038400030529     C**!!!              ELSE
038500030529     C**!!!              UPDATE    FISGN000
038600030529     C**!!!              MOVE      '1'           WDLSG1            1
038700030529     C**!!!              ENDIF
038800001130      *
038900030529     C**!!!              ENDSR
039000051012     c*---------------------------------------------------------------
039100051012     c* Deleto FNARS se non c'e' nemmeno la bolla arrivi
039200051012     c*---------------------------------------------------------------
039300051012     c     DELARS        BEGSR
039400051012     C     KARS          SETLL     FiarS01L
039500051012     C     KARS          READE     FiarS01L                               36
039600051012     c
039700051012    3C     *IN36         DOWEQ     *OFF
039800051012     c
039900051012     C                   DELETE    FiarS000
040000051012     C     KARS          READE     FiarS01L                               36
040100051012    3C                   ENDDO
040200051012     c
040300051012     c                   ENDSR
040400000801**      MSG
040500000801La bolla non è stata trovata
