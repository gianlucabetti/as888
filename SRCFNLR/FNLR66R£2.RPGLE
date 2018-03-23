000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941012     H* FNLR66R *----------------------------------------------------*
000300941012     H*      VISUALIZZAZIONE E CONTROLLO CAUSALI VARIAZIONI BOLLE
000301130405      *
000302130405      * PER COMPILARE E' NECESSARIO AVERE IN LISTA LA GAITRAGRPS (per TITAS)
000400000000     H*--------------------------------------------------------------*
000500001213     FFNLR66D   CF   E             WORKSTN USROPN
000600941012     F                                     SFILE(LR66DSF:NRR)
000700950120     FFNLBL01L  IF   E           K DISK
000800950120     FTABEL00F  IF   E           K DISK
000900941012     FFNBLP01L  IF   E           K DISK
001000000510     FAZORG01L  IF   E           K DISK
001100941012     FFNARB01L  IF   E           K DISK
001200991011     FFIAR601L  IF   E           K DISK
001300050331     FTigcp21L  IF   E           K DISK    RENAME(TIGCP000:TIGCP21)
001400050331     FTigcp01L  IF   E           K DISK
001500030124     fFiar501l  if   e           k Disk
001501130409     fFiari01l  if   e           k Disk
001502130405     fTitas30c  if   e           k Disk    extfile(wlibsede) UsrOpn
001600990715     D*--------------------------------------------------------------*
001700130419     D MSG             S             78    DIM(25) CTDATA PERRCD(1)             MSG DI ERRORE
001800950120     D K78             S              1    DIM(78)                              COMODO X MSG
001900021128
002000030124     d kAr5Trd         s                   Like(Ar5Trd) inz('BAN')
002100021128     d Wtc1            s                   Like(ArbTc1)
002200021128     d Wtc2            s                   Like(ArbTc2)
002300060301     d Wcbo            s                   Like(Arbcbo)
002400061026     d Wdata           s                   Like(Arbdcm)
002500060302     D nullPtr         S               *
002600060302     D prmAas          S
002700060302     D                                     LIKE(diz$Aas)
002800060302     D prmLnp          S
002900060302     D                                     LIKE(diz$Lnp)
003000060302     D prmNrs          S
003100060302     D                                     LIKE(diz$Nrs)
003200060302     D prmNsp          S
003300060302     D                                     LIKE(diz$Nsp)
003400060302     D prmFiv          S
003500060302     D                                     LIKE(diz$Fiv)
003600060302     D prmDft          S
003700060302     D                                     LIKE(diz$Dft)
003800060302     D prmNft          S
003900060302     D                                     LIKE(diz$Nft)
004000060302     D prmEsito        S             10I 0
004100061026     D flag            S              1
004200061026     D tipo            S              1
004300061026     D des28           S             28
004301130405
004302130405     D Wlibsede        S             21
004303130405     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS30C')
004304130405     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS30C')
004305130409     d Wtip            s                   Like(Aritip) inz('A')
004306130409     d ktbl            s                   Like(tastbl)
004307130418     d ktbl1           s                   Like(tastbl)
004400021128
004500941013     D***
004600941013     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004700941013     D***
004800941013     D WLBDAT          DS                  INZ
004900941013     D  G02DAT                 1      8  0
005000941013     D  G02INV                 9     16  0
005100941013     D  G02ERR                17     17
005200941013     D  G02TGI                18     22  0
005300000510     D OG143         E DS
005400000510     D DS7L          E DS
005500941013     D DS3A          E DS
005600920323     D KPJBA         E DS
005700941012     D*
005800941012     D* PASSAGGIO DATI
005900941012     D DSLR66        E DS                  EXTNAME(FNLR66DS)
006000941012     D*
006100941012     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
006200941012     D  DTU                  848    895P 0
006300941012     D                                     DIM(12)                              DATE PARAM
006400941012     D                                     PACKEVEN
006500030124     d dAr5Ban       e ds
006600061025     d ds7Lcampo     e ds
006700060302     D cndizion      E DS
006800060302     D                                     BASED(nullPtr)
006900060302     D                                     PREFIX(diz)
007000000000     C*---------------------------------------------------------------*
007100941012     C* INDICATORI
007200941012     C*---------------------------------------------------------------*
007300950331     C*  1    - INDICA SE PASSATA E TROVATA LA BOLLA DA DS
007400941012     C* 30    - DI COMODO
007500941012     C* 35    - PULIZIA ED EMISSIONE SFL
007600941013     C* 90    - ERORRE GENERICO
007700941012     C*---------------------------------------------------------------*
007800000000     C     *ENTRY        PLIST
007900000000     C                   PARM                    KPJBA
008000941012     C                   MOVEL     KPJBU         DSLR66
008100941012     C*
008200000000     C                   Z-ADD     1             CODUT
008300000000     C                   CALL      'XPARUT'
008400000000     C                   PARM                    UTEDSE
008500941012     C                   MOVEL     RAGUT         VIDDSA
008600000104     C                   MOVEL     DTU(10)       W0020             2 0
008700000104     C                   Z-ADD     DTU(10)       WDTU10
008800000104     C     W0020         IFGE      60
008900000104     C                   MOVEL     19            WDTU10
009000000104     C                   ELSE
009100000104     C                   MOVEL     20            WDTU10            8 0
009200000104     C                   ENDIF
009300941012     C*---------------------------------------------------------------*
009400941012     C* TIPO LANCIO, D15TLA "C" -           CHIUSO CON LR
009500941012     C* TIPO LANCIO, D15TLA "L" - ELABORO E CHIUDO CON LR
009600941012     C* TIPO LANCIO, D15TLA " " - ELABORO E CHIUDO IN RETRN
009700941012    1C     D66TLA        IFNE      'C'
009800941214     C                   SETOFF                                       90
009900001213    2C     D66GES        IFNE      'C'
009901140506    2C     D66GES        andne     'K'
010000001213    3C     OPN66         IFEQ      '0'
010100001213     C                   OPEN      FNLR66D
010200001213     C                   MOVE      '1'           OPN66
010300001213    3C                   ENDIF
010400001213   X2C                   ELSE
010500001213    3C     OPN66         IFEQ      '1'
010600001213     C                   CLOSE     FNLR66D
010700001213     C                   MOVE      '0'           OPN66
010800001213    3C                   ENDIF
010900001213    2C                   ENDIF
011000941012     C*
011100941012     C* VISUALIZZAZIONE PER SCELTA CODICE VARIAZIONE:
011200941013    2C     D66GES        IFEQ      'V'
011300941012     C* CARICO LE CAUSALI
011400920323     C                   EXSR      CARCOD
011500910219     C*
011600941013     C                   WRITE     LR66D02
011700900509     C     FOR01         TAG
011800941013     C                   EXFMT     LR66DCT
011900910912     C** CMD12 - RITORNO
012000920323     C  NKL              DO
012100910806     C* ENTER
012200920323     C                   EXSR      CONTR
012300941013     C     D66CVB        IFEQ      '  '
012400910219     C                   GOTO      FOR01
012500941013     C                   ENDIF
012600941013     C*
012700941013     C                   ENDDO
012800941013     C* SE IN VISUALIZZAZIONE TROVATO QUALCHE ERRORE NON MI INTERESSA
012900941013     C                   CLEAR                   D66MSG
013000941013     C*
013100941013  X2 C                   ELSE
013200941013     C*
013300941013     C* CONTROLLO LA CAUSALE VARIAZIONE ANCHE CON BOLLA SE IMMESSA
013400941013     C                   MOVEL     '7L'          COD
013500941013     C                   MOVEL     D66CVB        KEY
013600941013     C     KTAB2         CHAIN     TABEL                              30
013700941013     C*
013800941013    3C     *IN30         IFEQ      *ON
013900941013     C     *IN30         OREQ      *OFF
014000941013     C     TBLFLG        ANDNE     *BLANKS
014100941013     C                   MOVEL     MSG(2)        D66MSG
014200941013     C                   SETON                                        90
014300941013    3C                   ENDIF
014400941013     C*
014500941013    3C     *IN90         IFEQ      *OFF
014600941013     C                   MOVEL     TBLUNI        DS7L
014700941013     C                   MOVEL     §7LDES        D66DES
014800941013     C                   MOVEL     §7LFFI        D66FFI
014900941013     C                   MOVEL     §7LC4V        D66C4V
015000941013     C                   MOVEL     §7LSTF        D66STF
015100060130     C                   MOVEL     §7LPOCL       D66POCL
015200941013     C                   MOVEL     §7LFSA        D66FSA
015300941013     C                   MOVEL     §7LRTA        D66RTA
015400941102     C                   MOVEL     §7LBSF        D66BSF
015500130409     C                   MOVEL     §7LBFI        D66BFI
015600130409     C****               MOVEL     §7LFIN        D66BDI
015700060130     C                   MOVEL     §7LMTV        D66MTV
015701130411     C                   MOVEL     §7Lpda        D66pda
015800941013     C* CONTROLLO BOLLA
015900941013     C                   EXSR      CTRBOL
016000941013     c*
016100941013    4C     *IN90         IFEQ      *OFF
016200941013     C                   MOVEL     D66CVB        VIDCOD
016300941013     C                   EXSR      CTRCVB
016400941013    4C                   ENDIF
016500941013    3C                   ENDIF
016600941013     C*
016700941013    2C                   ENDIF
016800941013    1C                   ENDIF
016900910219     C*
017000941013     C     D66MSG        IFNE      *BLANKS
017100941013     C                   MOVEL     '1'           D66ERR
017200941013     C                   ENDIF
017300941013     C*
017400941013     C                   MOVEL     DSLR66        KPJBU
017500941013     C* CHIUSURA PGM
017600941013    1C     D66TLA        IFEQ      ' '
017700941013     C                   RETURN
017800941013     C                   ELSE
017900001213     C*
018000001213     C     OPN66         IFEQ      '1'
018100001213     C                   CLOSE     FNLR66D
018200001213     C                   ENDIF
018201130405
018202130405     c                   if        %open(titas30c)
018203130405     c                   close     titas30c
018204130405     c                   endif
018300001213     C*
018400941013     C                   SETON                                        LR
018500941013    1C                   ENDIF
018600941013     C*
018700941013     C* OPERAZIONI INIZIALI ------------------------------------------*
018800941013     C     *INZSR        BEGSR
018900941013     C     KTAB          KLIST
019000941013     C                   KFLD                    CODUT
019100941013     C                   KFLD                    COD
019200941013     C     KTAB2         KLIST
019300941013     C                   KFLD                    CODUT
019400941013     C                   KFLD                    COD
019500941013     C                   KFLD                    KEY
019600941013     C     KBOL          KLIST
019700941013     C                   KFLD                    D66AAS
019800941013     C                   KFLD                    D66LNP
019900941013     C                   KFLD                    D66NRS
020000941013     C                   KFLD                    D66NSP
020100991011     C     KBOL6         KLIST
020200991011     C                   KFLD                    D66AAS
020300991011     C                   KFLD                    D66LNP
020400991011     C                   KFLD                    D66NRS
020500991011     C                   KFLD                    D66NSP
020600991011     C                   KFLD                    WTRC
020700030124
020800030124     c     kAr5          Klist
020900030124     c                   Kfld                    D66Aas
021000030124     c                   Kfld                    D66Lnp
021100030124     c                   Kfld                    D66Nrs
021200030124     c                   Kfld                    D66Nsp
021300030124     c                   Kfld                    kAr5Trd
021301130405     C     Ktas          KLIST
021302130405     C                   KFLD                    D66AAS
021303130405     C                   KFLD                    D66LNP
021304130405     C                   KFLD                    D66NRS
021305130405     C                   KFLD                    D66NSP
021306130418     C                   KFLD                    ktbl1
021307130419     C     Ktas1         KLIST
021308130419     C                   KFLD                    D66AAS
021309130419     C                   KFLD                    D66LNP
021310130419     C                   KFLD                    D66NRS
021311130419     C                   KFLD                    D66NSP
021312130409     C     Kari          KLIST
021313130409     C                   KFLD                    D66AAS
021314130409     C                   KFLD                    D66LNP
021315130409     C                   KFLD                    D66NRS
021316130409     C                   KFLD                    D66NSP
021317130409     C                   KFLD                    wtip
021400941013     C* *LIKE DEFN
021500941013     C     *LIKE         DEFINE    TBLKEY        KEY
021600941013     C     *LIKE         DEFINE    TBLCOD        COD
021700941013     C     *LIKE         DEFINE    ARBLNA        WLNA
021800941013     C     *LIKE         DEFINE    ARBLNP        WLNP
021900941013     C     *LIKE         DEFINE    ARBDCM        WDCM
022000941013     C     *LIKE         DEFINE    ARBICC        WICC
022100941013     C     *LIKE         DEFINE    ARBDCP        WDCP
022200981221     C     *LIKE         DEFINE    ARBCCA        WCCA
022300991011     C     *LIKE         DEFINE    AR6TRC        WTRC
022400001213     C*
022500001213     C                   MOVE      '0'           OPN66             1
022600941013     C*
022700941013     C                   ENDSR
022800910205     C*
022900941012     C* CARICO LE CAUSALI VARIAIZONI ---------------------------------*
023000920323     C     CARCOD        BEGSR
023100941209     C                   CLEAR                   VIDSEL
023200941012     C*
023300941012     C                   SETON                                        35
023400941013     C                   WRITE     LR66DCT
023500941012     C                   SETOFF                                       35
023600941012     C*
023700910205     C                   Z-ADD     0             NRR               4 0
023800941013     C*
023900941013     C* CARICO I DATI BOLLA SE C'E'
024000941013     C                   EXSR      CTRBOL
024100941012     C*
024200910806     C                   SETOFF                                       30
024300920323     C                   MOVEL     '7L'          COD
024400920323     C     KTAB          SETLL     TABEL
024500920323     C     KTAB          READE     TABEL                                  30
024600910220     C*
024700941012    1C     *IN30         DOWEQ     *OFF
024800061031     c                   clear                   descon
024900061031     c                   clear                   desconp
025000920323     C*
025100941012     C* ESCLUDO GLI ANNULLATI
025200941012    2C     TBLFLG        IFEQ      ' '
025300920323     C                   MOVEL     TBLUNI        DS7L
025400941012     C                   MOVEL     TBLKEY        VIDCOD
025500941013     C                   MOVEL     §7LDES        DESCOD
025600941013     C                   SETOFF                                       90
025700941013     C* CONTROLLO CON BOLLA
025800950706     C                   MOVEL     VIDCOD        FLGCVB
025900941013     C                   EXSR      CTRCVB
026000941013     C*
026100941013    3C     *IN90         IFEQ      *OFF
026200941012     C*
026300941012     C* UTILIZZAZIONE IN PARTENZA
026400941013    4C     §7LUCP        IFNE      'N'
026500941012     C                   MOVEL     'SI'          VIDUCP
026600941012     C                   ELSE
026700941012     C                   MOVEL     '  '          VIDUCP
026800941012    4C                   ENDIF
026900941013     C*
027000941012     C* UTILIZZAZIONE IN ARRIVO
027100941013    4C     §7LUCV        IFNE      'N'
027200941012     C                   MOVEL     'SI'          VIDUCV
027300941012     C                   ELSE
027400941012     C                   MOVEL     '  '          VIDUCV
027500941012    4C                   ENDIF
027600060130     C* CONTROLLA ANCHE P.O. CLIENTE
027700060130    4C     §7LPOCL       IFEQ      'S'
027800060130     C                   MOVEL     'SI'          VIDPOCL
027900060130     C                   ELSE
028000060130     C                   MOVEL     '  '          VIDpocl
028100060130    4C                   ENDIF
028200060130     C
028300941013     C*
028400920323     C* VARIAZIONE DOPO CONSEGNA
028500061026     c                   movel     §7lvdco       ds7lcampo
028600061026    4C     §7Lvdopo      IFEQ      'S'
028700061026     C                   MOVEL     '  SI '       VIDVDC
028800061026     C                   ELSE
028900061026    5C     §7Lvdopo      IFEQ      'C'
029000061026     C                   MOVEL     ' SOLO'       VIDVDC
029100061026     C                   ELSE
029200061026     C                   MOVEL     '  NO '       VIDVDC
029300061026    5C                   END
029400061026    4C                   END
029500061026    4C**   §7LVDC        IFEQ      'S'
029600061026     C**                 MOVEL     ' SI '        VIDVDC
029700061026     C**                 ELSE
029800061026    5C**   §7LVDC        IFEQ      'C'
029900061026     C*+                 MOVEL     'SOLO'        VIDVDC
030000061026     C**                 ELSE
030100061026     C**                 MOVEL     ' NO '        VIDVDC
030200061026    5C**                 END
030300061026    4C**                 END
030400061026     c                   exsr      Decodi7LCAMPO
030500061026     c                   if        des28<>*blanks
030600061031     c
030700061031     c                   move      vidvdc        alfa4             4
030800061031     c                   if        %subst(vidvdc:2:1)=' '
030900061031     c                   movel     ' *'          vidvdc
031000061031     c                   else
031100061031     c                   movel     '*'           vidvdc
031200061031     c                   endif
031300061031     c
031400061031     c                   eval      descon='DopCON:'+alfa4
031500061031     c                   eval      descon=%trim(descon)+' '+des28
031600061026     c                   endif
031700941012     C*
031800941012     C* VARIAZIONE DOPO CONSEGNA PARZIALE
031900061026     c                   movel     §7lvdcop      ds7lcampo
032000061026    4C     §7Lvdopo      IFEQ      'S'
032100061031     C                   MOVEL     ' SI  '       VIDVDP
032200941012     C                   ELSE
032300061026    5C     §7Lvdopo      IFEQ      'C'
032400061031     C                   MOVEL     ' SOLO'       VIDVDP
032500941012     C                   ELSE
032600061031     C                   MOVEL     ' NO  '       VIDVDP
032700941013    5C                   END
032800941013    4C                   END
032900061026    4C**   §7LVDP        IFEQ      'S'
033000061026     C**                 MOVEL     ' SI '        VIDVDP
033100061026     C**                 ELSE
033200061026    5C**   §7LVDP        IFEQ      'C'
033300061026     C**                 MOVEL     'SOLO'        VIDVDP
033400061026     C**                 ELSE
033500061026     C**                 MOVEL     ' NO '        VIDVDP
033600061026    5C**                 END
033700061026    4C**                 END
033800061026     c**
033900061026     c                   exsr      Decodi7LCAMPO
034000061026     c
034100061026     c                   if        des28<>*blanks
034200061026     c                   movel     '*'           vidvdp
034300061026     c                   move      vidvdp        alfa4
034400061031     c                   eval      desconp='DopPARZ:'+alfa4+des28
034500061026     c                   endif
034600941012     C*
034700920323     C* VARIAZIONE DOPO CONTABILIZZAZIONE
034800941013    4C     §7LVDF        IFEQ      'S'
034900941014     C                   MOVEL     ' SI '        VIDVDF
035000920323     C                   ELSE
035100941013    5C     §7LVDF        IFEQ      'C'
035200920323     C                   MOVEL     'SOLO'        VIDVDF
035300920323     C                   ELSE
035400941014     C                   MOVEL     ' NO '        VIDVDF
035500941013    5C                   END
035600941013    4C                   END
035700941012     C*
035800941013     C* FRANCHI/ ASSEGNATI/ 2 BOLLA
035900941013     C                   CLEAR                   DESTBL
036000941013    4C     §7LVFA        IFEQ      'F'
036100060301     c                   select
036200060301     c                   when      §7lprp = 'S'
036300060301     c                   eval      destbl='FRANCO Prepag'
036400060301     c                   when      §7lprp = 'N'
036500060301     c                   eval      destbl='FRANCO NoPrep'
036600060301     c                   when      §7lprp = ' '
036700060301     c                   eval      destbl='FRANCO       '
036800060301     c*****              eval      destbl='FRANCO'
036900060301     c                   endsl
037000920323     C                   ELSE
037100941013    5C     §7LVFA        IFEQ      'A'
037200941013     C                   MOVEL     'ASSEG'       DESTBL
037300920323     C                   ELSE
037400941013    6C     §7LVFA        IFEQ      'E'
037500941013     C                   MOVEL     '2 BOL'       DESTBL
037600920323     C                   ELSE
037700941013     C                   MOVEL     'TUTTE'       DESTBL
037800941013    6C                   END
037900941013    5C                   END
038000941013    4C                   END
038100941013     C*
038200941013     C* VARIAZIONE PER BOLLE CON C/ASS
038300941013     C     §7LFCA        IFEQ      'S'
038400060126     c                   eval      destbl=%trim(destbl)+(' con C/A')
038500941013     C                   ENDIF
038600941013     C*
038700920323     C* PER BOLLA SEGUE FATTURA
038800941013    4C     §7LBSF        IFEQ      'S'
038900060126     c                   eval      destbl=%trim(destbl)+(' S.F.')
039000941013    4C                   END
039100920323     C* PER BOLLA FATTURA IMMEDIATA
039200941013    4C     §7LBFI        IFEQ      'S'
039300060126     c                   eval      destbl=%trim(destbl)+(' FT.IMM')
039400941013    4C                   END
039500060130     C* DA STORICIZZARE ora flag unico
039600941013    4C     §7LFSA        IFEQ      'S'
039700941012     C                   MOVEL     'SI'          VIDFSA
039800941012     C                   ELSE
039900941012     C                   MOVEL     '  '          VIDFSA
040000941013    4C                   END
040100060130     c
040200941012     C* DA STORICIZZARE ALLA PARTENZA
040300060130    4C**   §7LFSP        IFEQ      'S'
040400060130     C**                 MOVEL     'SI'          VIDFSP
040500060130     C**                 ELSE
040600060130     C**                 MOVEL     '  '          VIDFSP
040700060130    4C**                 END
040800060130     C* MEMORIZZAZIONE MOTIVO VARIAZIONE
040900060130    4C     §7LMTV        IFEQ      'S'
041000060130     C                   MOVEL     'SI'          VIDMTV
041100060130     C                   ELSE
041200060130     C                   MOVEL     '  '          VIDMTV
041300060130    4C                   END
041400910912     C*
041500941013     C                   MOVEL     §7LFFI        FLGFFI
041600941013     C                   MOVEL     §7LC4V        FLGC4V
041700941013     C                   MOVEL     §7LSTF        FLGSTF
041800060130     C                   MOVEL     §7LPOCL       FLGPOCL
041900941013     C                   MOVEL     §7LFSA        FLGFSA
042000941013     C                   MOVEL     §7LRTA        FLGRTA
042100060130     C                   MOVEL     §7LMTV        FLGMTV
042200910205     C* SCRIVO SUBFILE
042300910205     C                   ADD       1             NRR
042400941013     C                   WRITE     LR66DSF
042500941013    3C                   END
042600941013    2C                   END
042700910912     C*
042800920323     C     KTAB          READE     TABEL                                  30
042900941013    1C                   ENDDO
043000910205     C*
043100910205     C                   ENDSR
043200941013     C*
043300941013     C* CARICO LA BOLLA ----------------------------------------------*
043400941013     C     CTRBOL        BEGSR
043500941013     C                   SETOFF                                       01
043600941217     C                   CLEAR                   WGIAC
043700061026     C                   CLEAR                   ds3a
043800061026     C                   CLEAR                   Wfineksc1
043900061026     C                   CLEAR                   Wfineksc2
044000941013     C*
044100941013     C* SE IMMESSA LA BOLLA FACCIO CONTROLLO INCROCIATI
044200941013    1C     D66NSP        IFGT      0
044300941013     C* BOLLA ARRIVI
044400941013    2C     D66TBO        IFEQ      'A'
044500941013     C     KBOL          CHAIN     FNARB000                           30
044600941013     C*
044700941013    3C     *IN30         IFEQ      *OFF
044800941013     C                   SETON                                        01
044900060301     c                   movel     arbcbo        wcbo
045000991011     C                   MOVEL     '1'           WTRC
045100991011     C     KBOL6         CHAIN     FIAR6000                           30
045200941013     C  N30              MOVE      AR6DFT        WDFT                           D.FATT 1 BOL
045300941013     C   30              CLEAR                   WDFT
045400061026     c* codice segue fattura/fattura immediata
045500061026     c  n30              move      ar6ksc        wfineksc1         4 0
045600061026     c   30              move      arbksc        wfineksc1
045700941013     C                   MOVEL     ARBDCM        WDCM                           D.CONSEGNA
045800941013     C                   MOVEL     ARBDCP        WDCP                           D.CONS.PARZ
045900941013     C                   MOVEL     ARBLNP        WLNP                           LIN PARTENZA
046000941013     C                   MOVEL     ARBLNA        WLNA                           LIN ARRIVO
046100941013     C                   MOVEL     ARBICC        WICC                           INCASSO C/A
046200981221     C                   MOVEL     ARBCCA        WCCA                           CONS.ANOMALA
046300021128      * Imposto flag consegne particolari
046400021128     c                   Eval      Wtc1 = ArbTc1
046500021128     c                   Eval      Wtc2 = ArbTc2
046600021128
046700941013     C*
046800941013     C* DATA FATTURA: SE 2 BOLLA LA PRENDO DALLA ESTESIONE
046900941013     C                   MOVEL     '3A'          COD                            COD BOLLA
047000941013     C                   MOVEL(P)  ARBCBO        KEY
047100941013     C     KTAB2         CHAIN     TABEL                              31
047200941013     C  N31              MOVEL     TBLUNI        DS3A
047300941013     C*
047400941013     C* VEDO SE GIACENTE
047500050331     C     KBOL          CHAIN     Tigcp21L                           31        GIACENZA
047600941013    4C     *IN31         IFEQ      *OFF
047700050331     C     GCPFAS        ANDLT     40
047800941013     C     GCPATB        ANDEQ     ' '
047900941013     C                   MOVEL     'S'           WGIAC             1
048000941013    4C                   ENDIF
048100941013     C*
048200941013     C*GIRO DATA SPEDIZIONE
048300941013     C                   MOVEL     '3'           G02ERR
048400941013     C                   Z-ADD     ARBMGS        G02INV
048500941013     C                   MOVEL     ARBAAS        G02INV
048600941013     C                   CALL      'XSRDA8'
048700941013     C                   PARM                    WLBDAT
048800941013     C*
048900941013     C                   Z-ADD     G02DAT        VIDDSP
049000941013    3C                   ENDIF
049100941013     C*
049200941013   X2C                   ELSE
049300941013     C* BOLLA PARTENZA
049400941013     C     KBOL          CHAIN     FNBLP000                           30
049500941013     C*
049600941013    3C     *IN30         IFEQ      *OFF
049700941013     C                   SETON                                        01
049800060301     c                   movel     blpcbo        wcbo
049900991011     C                   MOVEL     '1'           WTRC
050000991011     C     KBOL6         CHAIN     FIAR6000                           30
050100000104     C  N30              MOVE      AR6DFT        WDFT              8 0          D.FATT 1 BOL
050200941013     C   30              CLEAR                   WDFT
050300061026     c* codice segue fattura/fattura immediata
050400061026     c  n30              move      ar6ksc        wfineksc1         4 0
050500061026     c   30              move      arbksc        wfineksc1
050600941013     C                   MOVEL     BLPDCM        WDCM                           D.CONSEGNA
050700941013     C                   MOVEL     BLPDCP        WDCP                           D.CONS.PARZ
050800941013     C                   MOVEL     BLPLNP        WLNP                           LIN PARTENZA
050900941013     C                   MOVEL     BLPLNA        WLNA                           LIN ARRIVO
051000981221     C                   MOVEL     BLPCCA        WCCA                           CONS.ANOMALA
051100021128      * Imposto flag consegne particolari
051200021128     c                   Eval      Wtc1 = BlpTc1
051300021128     c                   Eval      Wtc2 = BlpTc2
051400021128
051500941013     C                   CLEAR                   W2DFT                          D.FATT 2 BOL
051600941013     C* COME SE FOSSE SEMPRE INCASSATO: NON SI PUO' VARIARE
051700941013     C                   MOVEL     'S'           WICC                           INCASSO C/A
051800941013     C* DATA FATTURA: SE 2 BOLLA LA PRENDO DALLA ESTESIONE
051900941013     C                   MOVEL     '3A'          COD
052000941013     C                   MOVEL(P)  BLPCBO        KEY
052100941013     C     KTAB2         CHAIN     TABEL                              31
052200941013     C  N31              MOVEL     TBLUNI        DS3A
052300991011     C*
052400941013     C* VEDO SE GIACENTE
052500050331     C     KBOL          CHAIN     Tigcp01L                           31
052600941013    4C     *IN31         IFEQ      *OFF
052700950706     C     GCPFAS        ANDLT     60
052800941013     C     GCPATB        ANDEQ     ' '
052900941013     C                   MOVEL     'S'           WGIAC             1
053000941013    4C                   ENDIF
053100941013     C*
053200941013     C*GIRO DATA SPEDIZIONE
053300941013     C                   MOVEL     '3'           G02ERR
053400941013     C                   Z-ADD     BLPMGS        G02INV
053500941013     C                   MOVEL     BLPAAS        G02INV
053600941013     C                   CALL      'XSRDA8'
053700941013     C                   PARM                    WLBDAT
053800941013     C*
053900941013     C                   Z-ADD     G02DAT        VIDDSP
054000941013    3C                   ENDIF
054100941013     C*
054200941013    2C                   ENDIF
054300061026     c
054400061026     c* Chain su seconda bolla, se presente
054500061026    4C     §3ATB2        IFNE      *BLANKS
054600061026     C                   MOVEL     '2'           WTRC
054700061026     C     KBOL6         CHAIN     FIAR6000                           32
054800061026     C  N32              MOVE      AR6DFT        W2DFT             8 0          D.FATT 2 BOL
054900061026     C   32              CLEAR                   W2DFT
055000061026     c  n32              move      ar6ksc        wfineksc2         4 0
055100061026    4C                   ENDIF
055200941013     C*
055300941013     C* NON TROVATA BOLLA
055400941013    2C     *IN01         IFEQ      *OFF
055500060301     c                   clear                   wcbo
055600941013     C                   MOVEL     MSG(1)        D66MSG
055700941013     C                   SETON                                        90
055800941013   X2C                   ELSE
055900941013     C*
056000941013     C* IMPOSTO I FLAG CHE MI SERVONO PER I CONTROLLI
056100941013     C                   MOVEL     VIDCOD        FLGCVB            1
056200941013     C                   MOVEL     §3ATB1        FLGTB1            1
056300941013    2C                   ENDIF
056400030124
056500060301     c* se bolla avente codice "M"=C/S per prepagati annullati
056600060301     c* non è possibile effettuare alcuna variazione
056700060301     c                   if        wcbo = 'M '
056800060301     C                   MOVEL     MSG(13)       D66MSG
056900060301     C                   SETON                                        90
057000060301     c                   endif
057100030124      * Se richiesta causale x Bancali la bolla deve avere il record 'BAN'
057200030124      * in Fira5 e non deve essere già stato mod.
057300030124     c                   If        D66Cvb = 'BR'
057400030124     c                   Clear                   dAr5Ban
057500030124     c     Kar5          Chain     Fiar501l
057600030124     c                   If        Not %Found(Fiar501l)
057700030127     c                   Movel     Msg(21)       D66Msg
057800030124     c                   Eval      *In90 = *On
057900030124     c                   Else
058000030124     c                   Movel     Ar5Uni        dAr5Ban
058100030124     c                   If        §Ar5Rb1 > *Zeros or §Ar5Rb2 > *Zeros
058200030127     c                   Movel     Msg(22)       D66Msg
058300030124     c                   Eval      *In90 = *On
058400030124     c                   EndIf
058500030124     c                   EndIf
058600030124     c                   EndIf
058700941013     C*
058800941013    1C                   ENDIF
058900920323     C*
059000941013     C                   ENDSR
059100941013     C*
059200941013     C* CONTROLLO LA CAUSALE CON LA BOLLA IMMESSA --------------------*
059300941013     C     CTRCVB        BEGSR
059400941013     C*
059500941013     C                   SETOFF                                       90
059600000510     C*
059700000510     C     WLNP          CHAIN     AZORG01L                           30
059800000510     C  N30              MOVEL     ORGDE3        OG143
059900000510     C   30              CLEAR                   OG143
060000941013     C*
060001140502     c* i controlli seguenti li ignoro per le causali utilizzate solo
060002140502     c* per chiamate cieche (entrambi i codici UCP e UCV='N')
060003140506     c* per dare la possibilità al chiamante di richiamare LR66R per
060004140502     c* esecuzione dei controlli
060005140506     c                   if        d66ges<>'K' or §7lucv<>'N' or §7lucp<>'N'
060100941013     C* ESCLUDO LE BOLLE DA NON UTILIZZARE SE SO IL TIPO DI BOLLA
060200941013    1C     D66TBO        IFEQ      'A'
060300941014     C     §7LUCV        ANDEQ     'N'
060400941013     C                   SETON                                        90
060500941013     C                   MOVEL     MSG(3)        D66MSG
060600941013    1C                   ENDIF
060700941013     C*
060800941013    1C     D66TBO        IFEQ      'P'
060900941014     C     §7LUCP        ANDEQ     'N'
061000941013     C                   SETON                                        90
061100941013     C                   MOVEL     MSG(3)        D66MSG
061200941013    1C                   ENDIF
061201140502
061202140502     c                   endif
061300941013     C*
061400941013    1C     *IN01         IFEQ      *ON
061500941013     C     *IN90         ANDEQ     *OFF
061600941013     C*
061700941013     C* SE GIACENTE ESCLUDO LA VARIAZIONE DI C/A
061800011121      * PER LE BOLLE ARRIVI
061900941013    2C     WGIAC         IFEQ      'S'
062000011121     C     D66TBO        ANDEQ     'A'
062100941013     C     FLGCVB        ANDEQ     'K'
062200011120     C     VIDCOD        ANDNE     'KA'
062300011120     C     VIDCOD        ANDNE     'KP'
062400941013     C                   SETON                                        90
062500941013     C                   MOVEL     MSG(4)        D66MSG
062600941013     C                   GOTO      ENDCCV
062700941013    2C                   ENDIF
062800011121      * PER LE BOLLE PARTENZA
062900011121    2C     WGIAC         IFEQ      'S'
063000011121     C     D66TBO        ANDEQ     'P'
063100011121     C     FLGCVB        ANDEQ     'K'
063200011121     C                   SETON                                        90
063300011121     C                   MOVEL     MSG(4)        D66MSG
063400011121     C                   GOTO      ENDCCV
063500011121    2C                   ENDIF
063600941013     C**
063700941013     C* F L A G   C / A S S E G N O
063800941013     C**
063900941013     C* SE NON C'E' C/A  O GIA' INCASSATO NON POSSO USARE LA KA/KB
064000941013    2C     §7LFCA        IFEQ      'S'
064100941013    3C     §3AFCA        IFEQ      0
064200950119     C     WICC          ORNE      ' '
064300950119     C     WICC          ANDNE     'R'
064400941013     C                   SETON                                        90
064500941013     C                   MOVEL     MSG(5)        D66MSG
064600941013     C                   GOTO      ENDCCV
064700941013    3C                   ENDIF
064800950120     C* SE C'E IL FLAG C/A MA E' BOLLA LEGATA IN LOCALE --> DEVO
064900950120     C*  VARIARE LA MAMMA
065000950120    3C     §3AFCA        IFNE      0
065100950120     C     KBOL          CHAIN     FNLBL01L                           30
065200950120    4C     *IN30         IFEQ      *OFF
065300950120     C     LBLLAP        ANDEQ     LBLLAN
065400950120     C                   SETON                                        90
065500950120     C* PREPARO IL MESSAGGIO
065600950120     C                   MOVEA     MSG(16)       K78
065700950120     C                   MOVE      LBLAAP        W002A             2
065800950120     C                   MOVEA     W002A         K78(59)
065900950120     C                   MOVEL     LBLLPP        W003A             3
066000950120     C                   MOVEA     W003A         K78(62)
066100950120     C                   MOVE      LBLNRP        W002A
066200950120     C                   MOVEA     W002A         K78(66)
066300950120     C                   MOVE      LBLNSP        W007A             7
066400950120     C                   MOVEA     W007A         K78(69)
066500950120     C*
066600950120     C                   MOVEA     K78           D66MSG
066700950120     C                   GOTO      ENDCCV
066800950120    4C                   ENDIF
066900950120    3C                   ENDIF
067000950120    2C                   ENDIF
067100000510     C* PER BOLLE POSTE
067200000510     C     §OGPT         IFEQ      'S'
067300000510     C     §7LFPT        ANDEQ     ' '
067400000510     C                   SETON                                        90
067500000510     C                   MOVEL     MSG(19)       D66MSG
067600000510     C                   GOTO      ENDCCV
067700000510    3C                   ENDIF
067800941013     C**
067900941013     C* T I P O   D I   B O L L A
068000941013     C**
068100941013    2C     §7LVFA        IFEQ      'F'
068200060301     C***  FLGTB1        ANDNE     'F'
068300060301     c                   if        flgtb1 <> 'F'  or
068400060301     c                             (§7lprp =  'S'  and wdft = 0 ) or
068500060301     c                             (§7lprp =  'N'  and wdft > 0 )
068600941013     C                   SETON                                        90
068700060301     c                   endif
068800941013    2C                   ENDIF
068900941013    2C     §7LVFA        IFEQ      'A'
069000941013     C     FLGTB1        ANDNE     'A'
069100941013     C                   SETON                                        90
069200941013    2C                   ENDIF
069300941013    2C     §7LVFA        IFEQ      'E'
069400941013     C     §3ATB2        ANDEQ     *BLANKS
069500941013     C                   SETON                                        90
069600941013    2C                   ENDIF
069700941013     C*
069800941013    2C     *IN90         IFEQ      *ON
069900061025     C                   MOVEL     MSG(13)       D66MSG
070000941013     C                   GOTO      ENDCCV
070100941013    2C                   ENDIF
070200941013     C**
070300941013     C* C A M B I O   C O D I C E   B O L L A
070400941013     C**
070500941013     C* PER TIPI BOLLA CREATI IN AUTOMATICO NON E' POSSIBILE
070600941013     C* SE E' DIROTTAMENTO AD ALTRA FILIALE, SI
070700941013    2C     VIDCOD        IFEQ      'K8'
070800941013     C     VIDCOD        OREQ      'K5'
070900941013     C*
071000941013    3C     §3AUCB        IFEQ      'N'
071100941013     C     WLNP          ANDEQ     WLNA
071200941013     C                   SETON                                        90
071300941013     C                   MOVEL     MSG(6)        D66MSG
071400941013     C                   GOTO      ENDCCV
071500941013    3C                   ENDIF
071600981221     C* SE E' CAMBIO DI PORTO NON POSSO MODIFICARE IL TIPO BOLLA
071700981221    3C     §3AUCB        IFEQ      'N'
071800981221     C     WCCA          ANDEQ     '9'
071900981221     C                   SETON                                        90
072000981221     C                   MOVEL     MSG(18)       D66MSG
072100981221     C                   GOTO      ENDCCV
072200981221    3C                   ENDIF
072300941013    2C                   ENDIF
072400941013     C**
072500941013     C* F L A G   C O N S E G N A
072600941013     C**
072700061025     c* Metto il flag in DS
072800061026     c                   movel     §7lvdco       ds7lcampo
072900061026     c* Imposto il camp data con la data consegna
073000061026     c                   eval      Wdata=Wdcm
073100061026     c                   Exsr      CTRds7lcampo
073200061026     c
073300061026     c                   if        tipo='P' and flag='1'
073400061026     C                   MOVEL     MSG(7)        D66MSG
073500061026     C                   GOTO      ENDCCV
073600061026    2C                   ENDIF
073700061026     c                   if        tipo='D' and flag='1'
073800061026     C                   MOVEL     MSG(8)        D66MSG
073900061026     C                   GOTO      ENDCCV
074000061026    2C                   ENDIF
074100061026     c                   if        tipo='D' and flag='2'
074200061026     C                   MOVEL     MSG(23)       D66MSG
074300061026     C                   GOTO      ENDCCV
074400061026    2C                   ENDIF
074500061026     c
074600941013     C* NON VARIABILE DOPO CONSEGNA
074700061025    2C***  §7LVDC        IFEQ      ' '
074800061025     C***  §7LVDC        OREQ      'N'
074900061025    3C***  WDCM          IFGT      0
075000061025     C***                SETON                                        90
075100061025     C***                MOVEL     MSG(7)        D66MSG
075200061025     C***                GOTO      ENDCCV
075300061025    3C***                ENDIF
075400061025    2C***                ENDIF
075500061025     C***
075600941013     C* VARIABILE SOLO DOPO LA CONSEGNA
075700061025    2C***  §7LVDC        IFEQ      'C'
075800061025     C***  WDCM          ANDEQ     0
075900061025     C***                SETON                                        90
076000061025     C***                MOVEL     MSG(8)        D66MSG
076100061025     C***                GOTO      ENDCCV
076200061025    2C***                ENDIF
076300941013     C**
076400941013     C* F L A G   C O N S E G N A   P A R Z I A L E
076500941013     C**
076600061026     C**
076700061102     c* lo controllo solo se la data consegna = 0 altrimenti non lo faccio
076800061102     c                   if        wdcm=0
076900061026     c* Metto il flag in DS
077000061026     c                   movel     §7lvdcop      ds7lcampo
077100061026     c* Imposto il camp data con la data consegna
077200061026     c                   eval      Wdata=Wdcp
077300061026     c                   Exsr      CTRds7lcampo
077400061026     c
077500061026     c                   if        tipo='P' and flag='1'
077600061026     C                   MOVEL     MSG(9)        D66MSG
077700061026     C                   GOTO      ENDCCV
077800061026    2C                   ENDIF
077900061026     c                   if        tipo='D' and flag='1'
078000061026     C                   MOVEL     MSG(8)        D66MSG
078100061026     C                   GOTO      ENDCCV
078200061026    2C                   ENDIF
078300061026     c                   if        tipo='D' and flag='2'
078400061026     C                   MOVEL     MSG(24)       D66MSG
078500061026     C                   GOTO      ENDCCV
078600061026    2C                   ENDIF
078700061102    2C                   ENDIF
078800941013     C* NON VARIABILE DOPO CONSEGNA PARZIALE
078900061026    2C**   §7LVDP        IFEQ      ' '
079000061026     C**   WDCP          ANDGT     0
079100061026     C**                 SETON                                        90
079200061026     C**                 MOVEL     MSG(9)        D66MSG
079300061026     C**                 GOTO      ENDCCV
079400061026    2C**                 ENDIF
079500941013     C*
079600941013     C* VARIABILE SOLO DOPO LA CONSEGNA PARZIALE
079700061026    2C**   §7LVDP        IFEQ      'C'
079800061026     C**   WDCP          ANDEQ     0
079900061026     C**                 SETON                                        90
080000061026     C**                 MOVEL     MSG(8)        D66MSG
080100061026     C**                 GOTO      ENDCCV
080200061026    2C**                 ENDIF
080300941013     C**
080400941013     C* F L A G   C O N T A B I L I Z Z A Z I O N E
080500941013     C**
080600941013     C* PRENDO LA DATA FATTURA DELLA 2 BOLLE SE
080700941013     C*  1) CAUSALE INIZIA PER 'K'
080800941013     C*  2) SE E' PROPRIO PER 2 BOLLA
080900941013     C*  3) SE E' VARIAZIONE DEL DESTINATARIO
081000941013    2C     §7LVFA        IFEQ      'E'
081100941013     C*
081200941013     C     §3ATB2        ORNE      *BLANKS
081300941013     C     FLGCVB        ANDEQ     'K'
081400941013     C*
081500950301     C     §3ATB2        ORNE      *BLANKS
081600950301     C     VIDCOD        ANDEQ     'I0'
081700000104     C                   MOVEL     W2DFT         WDFTC             8 0
081800941013   X2C                   ELSE
081900941013     C                   MOVEL     WDFT          WDFTC
082000941013    2C                   ENDIF
082100941013     C*
082200941013     C* NON VARIABILE DOPO LA CONTABILIZZAZIONE
082300941013    2C     WDFTC         IFGT      0
082400941013     C     §7LVDF        ANDEQ     ' '
082500000104     C     WDFTC         ANDLE     WDTU10
082600941013     C                   SETON                                        90
082700941013     C                   MOVEL     MSG(11)       D66MSG
082800941013     C                   GOTO      ENDCCV
082900941013    2C                   ENDIF
083000941013     C*
083100941013     C* VARIABILE SOLO DOPO LA CONTABILIZZAZIONE
083200941013    2C     §7LVDF        IFEQ      'C'
083300000104    3C     WDFTC         IFGT      WDTU10
083400941013     C     WDFTC         OREQ      0
083500941013     C                   SETON                                        90
083600941013     C                   MOVEL     MSG(12)       D66MSG
083700941013     C                   GOTO      ENDCCV
083800941013    3C                   ENDIF
083900941013    2C                   ENDIF
083901130410     c* Non variabile dopo contabilizzazione/incasso per fatture immediate
083904130405     c                   exsr      opnfile
083905130614     c***                if        §7lbfi='S' or §7lvdf=' '
083906130410     c* Setll su Titas senza tipo bolla e se bolla non esiste
083907130410     c* sicuramente non è contabilizzata e quindi permetto la variazione
083908130409     c     kbol          setll     titas30c
083909130409     c                   if        %equal(titas30c)
083910130409     c                   clear                   ktbl
083911130409     c                   select
083912130409     c* bolla variabile: franco o assegnato chaino con tipo bolla §3atb1
083913130409     c                   when      §7lvfa='F' or §7lvfa='A'
083914130405     c                   eval      ktbl=§3atb1
083915130409     c                   exsr      chtitas
083916130409     c   90              goto      endccv
083921130409     c* bolla variabile: estensione chaino con tipo bolla §3atb2
083922130409     c                   when      §7lvfa='E'
083923130409     c                   eval      ktbl=§3atb2
083924130409     c                   exsr      chtitas
083925130409     c   90              goto      endccv
083926130409     c                   other
083927130409     c* bolla variabile: tutte chaino con §3atb1 e §3atb2 se presente
083928130409     c                   eval      ktbl=§3atb1
083929130409     c                   exsr      chtitas
083930130409     c   90              goto      endccv
083931130409     c                   if        §3atb2<>*blanks
083932130409     c                   eval      ktbl=§3atb2
083933130409     c                   exsr      chtitas
083934130409     c   90              goto      endccv
083935130409     c                   endif
083936130409     c                   endsl
083937130409     c
083938130409     c                   endif
083939130614     c***                endif
084000941013     C**
084100941013     C* P E R   B O L L A     S E G U E   F A T T U R A
084200941013     C**
084300941013     C     §7LBSF        IFEQ      'S'
084400941013     C     WDFTC         ANDGT     0
084500941013     C                   SETON                                        90
084600941013     C                   MOVEL     MSG(14)       D66MSG
084700941013     C                   GOTO      ENDCCV
084800941013     C                   ENDIF
084900941013     C**
085000941013     C* P E R   B O L L A   F A T T U R A   I M M E D I A T A
085100941013     C**
085200941013     C     §7LBFI        IFEQ      'S'
085300941013     C     WDFTC         ANDEQ     0
085400941013     C                   SETON                                        90
085500941013     C                   MOVEL     MSG(15)       D66MSG
085600941013     C                   GOTO      ENDCCV
085700941013     C                   ENDIF
085800070305     c
085900070305     c* Non faccio utilizzare se cpdioce bollo storno assegnato
086000070305     c                   if        §7lBFI='S' or §7lBSF='S'
086100070305     c                   if        (§7lvfa='A' and §3atb1='AS') or
086200070305     c                             (§7lvfa='E' and §3atb2='AP')
086300070305     C                   SETON                                        90
086400070305     C                   MOVEL     MSG(25)       D66MSG
086500070305     C                   GOTO      ENDCCV
086600070305     C                   ENDIF
086700070305     c                   endif
086800070305     c
086900950331     C* SE GIA INCASSATA NON SI PUO' UTILIZZARE LA CAUSALE PER
087000990415     C* FATTURE IMMEDIATE SE LA CAUASALE NON LO PERMETTE
087100130410     C***  §7LBFI        IFEQ      'S'
087200130410     C***  §7LFIN        ANDNE     'S'
087300130410     C***  ARBICA        ifne      ' '
087400130410     C***  ARBICA        ANDNE     'R'
087500130410     C***                SETON                                        90
087600130410     C***                MOVEL     MSG(17)       D66MSG
087700130410     C***                GOTO      ENDCCV
087701130410     c***                endif
087702130410     c***  arbica        ifeq      'R'
087703130410     C***  KARI          CHAIN     FIARI01L
087704130410     c***                if        %found(fiari01l) and ariatb=' '
087705130410     C***                SETON                                        90
087706130410     C***                MOVEL     MSG(17)       D66MSG
087707130410     C***                GOTO      ENDCCV
087708130410     c***                endif
087709130410     c****               endif
087800130410     C****               ENDIF
087900060302     c* per bolle partenze (prepagati)
088000060303     c                   if        §7lvfa = 'F' and §7lprp = 'S'
088100060302     c                   z-add     blpaas        prmaas
088200060302     c                   z-add     blplnp        prmlnp
088300060302     c                   z-add     blpnrs        prmnrs
088400060302     c                   z-add     blpnsp        prmnsp
088500060302     c                   z-add     ar6fiv        prmfiv
088600060302     c                   z-add     ar6nft        prmnft
088700060302     c                   z-add     wdft          prmdft
088800060302     c                   clear                   prmesito
088900060608     c                   call      'YCOW0121R1'
089000060302     C                   PARM                    prmaas
089100060302     C                   PARM                    prmLnp
089200060302     C                   PARM                    prmNrs
089300060302     C                   PARM                    prmNsp
089400060302     C                   PARM                    prmFiv
089500060302     C                   PARM                    prmDft
089600060302     C                   PARM                    prmNft
089700060302     C                   PARM                    prmEsito
089800060302     c                   if        prmesito <> 0
089900060302     C                   MOVEL     msg(17)       d66msg
090000060302     C                   SETON                                        90
090100060302     C                   GOTO      endccv
090200060302     c                   endif
090300060302     c                   endif
090400950331     C* SE BOLLA IN ASSEGNATO MA NON ANCORA ABILITATO, NON POOSO UTILIZ
090500941013     C*  UNA CAUSALE DI TASSAZIONE RITASSAZIONE BOLLA
090600060303     C     ARBACA        ifeq      ' '
090700941013     C     §7LRTA        IFEQ      'S'
090800941013     C     §7LBFI        OREQ      'S'
090900941013     C     §7LBSF        OREQ      'S'
091000941013     C                   SETON                                        90
091100941013     C                   MOVEL     MSG(10)       D66MSG
091200941013     C                   GOTO      ENDCCV
091300941013     C                   ENDIF
091400941013     C                   ENDIF
091500021128      **
091600021128      * C O N S E G N A   P A R T I C O L A R E
091700021128      **
091800021128      * Se presente la  particolare "Supermercati" la causale deve
091900021128      * essere abilitata
092000021128     c                   If         §7lSup = 'N' and
092100021128     c                             (Wtc1 = 'S' or Wtc2 = 'S')
092200021128     c                   Eval      *In90 = *On
092300021128     c                   Eval      D66Msg = Msg(20)
092400021128     c                   GoTo      EndCcv
092500021128     c                   endif
092600941013     C*
092700941013     C                   ENDIF
092800941013     C*
092900941013     C     ENDCCV        ENDSR
093000061026     c*
093100061026     c* controllo DS7Lcampo  ---------------------------------------------*
093200061026     c     CTRds7lcampo  BEGSR
093300061026     c                   clear                   tipo
093400061026     c                   clear                   flag
093500061026     c* 1° flag --> §7LVDOPO variabile prima o dopo
093600061026     c*             Non variabile DOPO
093700061026    2c                   if        (§7lvdopo =' ' or §7lvdopo='N') and
093800061026     c                             wdata>0
093900061026     c                   movel     '1'           flag
094000061026     c                   movel     'P'           tipo
094100061026     c                   seton                                        90
094200061026     C                   GOTO      ENDctr
094300061026    2C                   ENDIF
094400061026     c
094500061026     c*             Variabile SOLO DOPO
094600061026    2c                   if        §7lvdopo ='C' and wdata=0
094700061026     c                   movel     '1'           flag
094800061026     c                   movel     'D'           tipo
094900061026     C                   SETON                                        90
095000061026     C                   GOTO      ENDctr
095100061026    2C                   ENDIF
095200061108     c
095300061108     c                   if        wdata>0
095400061026     c*
095500061026     c* 2° flag --> se pieno Include o ESclude alcuni tipi bolla
095600061026     c                   setoff                                       90
095700061108     c
095800061026     c* 3° flag --> tipo bolla da includere/escludere
095900061026    2c                   if        §7lines='I'
096000061026    3c                   if        §7lfras='F'  and flgtb1<>'F'
096100061026     C                   SETON                                        90
096200061026    3c                   endif
096300061026    3c                   if        §7lfras='A'  and flgtb1<>'A'
096400061026     C                   SETON                                        90
096500061026    3c                   endif
096600061026    3c                   if        §7lfras='E'  and §3atb2='  '
096700061026     C                   SETON                                        90
096800061026    3c                   endif
096900061026    3c                   if        §7lfras='S'  and §3atb2='  '
097000061026     c                             and flgtb1='F'
097100061026     C                   SETON                                        90
097200061026    3c                   endif
097300061026     c
097400061026     c* 4° flag -->  includi codificati o non
097500061026    3c                   if        not *in90 and §7lcodif='C'
097600061026     c                   EXSR      Codificato
097700061026    3c                   endif
097800061026     c
097900061026    3c                   if        not *in90 and §7lcodif='N'
098000061026     c                   exsr      NonCodificato
098100061026    3c                   endif
098200061026     c
098300061026    2c                   endif
098400061026    c
098500061026    2c                   if        §7lines='E'
098600061026    3c                   if        §7lfras='F'  and flgtb1='F'
098700061026     C                   SETON                                        90
098800061026    3c                   endif
098900061026    3c                   if        §7lfras='A'  and flgtb1='A'
099000061026     C                   SETON                                        90
099100061026    3c                   endif
099200061026    3c                   if        §7lfras='E'  and §3atb2<>'  '
099300061026     C                   SETON                                        90
099400061026    3c                   endif
099500061026    3c                   if        §7lfras='S'  and (§3atb2<>'  '
099600061026     c                             or  flgtb1='A')
099700061026     C                   SETON                                        90
099800061026    3c                   endif
099900061026    c
100000061026     c* 4° flag --> esclcudi codificati o non
100100061026    3c                   if        not *in90 and §7lcodif='C'
100200061026     c* Devo escludere i non codificati
100300061026     c                   exsr      NonCodificato
100400061026    3c                   endif
100500061026     c
100600061026    3c                   if        not *in90 and §7lcodif='N'
100700061026     c                   exsr      COdificato
100800061026    3c                   endif
100900061026     c
101000061026    2c                   endif
101100061026     C*
101200061026    2C     *IN90         IFEQ      *ON
101300061026     c                   movel     '2'           flag
101400061026     c                   movel     'D'           tipo
101500061026     C                   GOTO      ENDctr
101600061026    2C                   ENDIF
101700061108    2C                   ENDIF
101800061026     c     ENDCTR        ENDSR
101900061026     c
102000061026     c* OK se bolla     codificata ----------------------------------*
102100061026     c     Codificato    BEGSR
102200061026     c*  di 1°bolla
102300061026    4c                   if        §7lfras<>'E' and (wfineksc1=8888 or
102400061026     c                                              wfineksc1=9999)
102500061026     c                   seton                                        90
102600061026    4c                   endif
102700061026     c*  di 2°bolla
102800061026    4c                   if        (§7lfras='E' or §7lfras='S' or
102900061026    4c                              §7lfras=' ') and
103000061026     c                             wfineksc2=9999
103100061026     c                   seton                                        90
103200061026    4c                   endif
103300061026     c                   ENDSR
103400061026     c
103500061026     c* OK se bolla non codificata -----------------------------------*
103600061026     c     nonCodificato begsr
103700061026     c*  di 1°bolla
103800061026    4c                   if        §7lfras<>'E' and (wfineksc1<>8888 and
103900061026     c                                              wfineksc1<>9999)
104000061026     c                   seton                                        90
104100061026    4c                   endif
104200061026     c*  di 2°bolla
104300061026    4c                   if        (§7lfras='E' or §7lfras='S' or
104400061026    4c                              §7lfras=' ') and
104500061026     c                             wfineksc2<>9999 and wfineksc2>0
104600061026     c                   seton                                        90
104700061026    4c                   endif
104800061026     c                   ENDSR
104900061026     c*
105000061026     c* Decodifica la DS7Lcampo ------------------------------------------*
105100061026     c     Decodi7LCAMPO BEGSR
105200061026     c                   clear                   des28
105300061031     c                   if        §7lines<>' '
105400061031     c
105500061026     c                   if        §7lines='I'
105600061026     c                   movel     'Includi'     des28
105700061026     c                   endif
105800061026     c                   if        §7lines='E'
105900061026     c                   movel     'Escludi'     des28
106000061026     c                   endif
106100061026     c
106200061026     c                   select
106300061026     c                   when      §7lfras='F'
106400061031     c                   eval      des28=%trim(des28)+' Franchi '
106500061026     c                   when      §7lfras='A'
106600061031     c                   eval      des28=%trim(des28)+' Asseg.1Bol'
106700061026     c                   when      §7lfras='E'
106800061031     c                   eval      des28=%trim(des28)+' Asseg.2Bol'
106900061026     c                   when      §7lfras='S'
107000061031     c                   eval      des28=%trim(des28)+' Asseg1/2Bo'
107100061026     c                   when      §7lfras=' '
107200061031     c                   eval      des28=%trim(des28)+' TutteBolle '
107300061026     c                   endsl
107400061026     c
107500061026     c                   if        §7lcodif='C'
107600061031     c                   eval      des28=%trim(des28)+' Codific'
107700061026     c                   endif
107800061026     c                   if        §7lcodif='N'
107900061031     c                   eval      des28=%trim(des28)+' NoCodif'
108000061026     c                   endif
108100061031     c                   endif
108200061026     c                   ENDSR
108300941013     C*
108400060302     C* CONtROLLO SE EFFETTUATA SELEZIONE ----------------------------*
108500920323     C     CONTR         BEGSR
108600941013     C                   READC     LR66DSF                                31
108700920323     C*
108800941013     C     *IN31         DOWEQ     *OFF
108900941013     C*
109000920323     C     VIDSEL        IFEQ      '1'
109100941013     C                   MOVEL     VIDCOD        D66CVB
109200941013     C                   MOVEL     DESCOD        D66DES
109300941013     C                   MOVEL     FLGFFI        D66FFI
109400941013     C                   MOVEL     FLGC4V        D66C4V
109500941013     C                   MOVEL     FLGSTF        D66STF
109600060130     C                   MOVEL     FLGPOCL       D66POCL
109700941013     C                   MOVEL     FLGFSA        D66FSA
109800941013     C                   MOVEL     FLGRTA        D66RTA
109900060130     C                   MOVEL     FLGMTV        D66MTV
110000920323     C                   SETON                                        31
110100941013     C                   ENDIF
110200920323     C*
110300941013     C  N31              READC     LR66DSF                                31
110400941013     C                   ENDDO
110500920323     C                   ENDSR
110501130405     C* Apertura titas -----------------------------------------------*
110502130405     C     opnfile       BEGSR
110503130405     c                   if        not %open(titas30c)
110504130405     c                   if        %subst(knsif:7:1)='P'
110505130405     c                   eval      wlibsede=wlibsedep
110506130405     c                   else
110507130405     c                   eval      wlibsede=wlibsedeb
110508130405     c                   endif
110509130405     c                   open      titas30c
110510130405     c                   endif
110511130405     c                   endsr
110512130409     C* Chain titas  per controllo se contabilizzata -----------------*
110513130409     C     chtitas       BEGSR
110514130418     c                   eval      ktbl1=%subst(ktbl:1:1)
110515130418     c     ktas          setll     titas30c
110516130418     c     ktas1         reade     titas30c
110517130419    1c                   if        not %eof(titas30c) and
110518130419     c                             %subst(tastbl:1:1)=ktbl1
110519130418
110526130410     c* BOLLA CONTABILIZZATA
110527130410     c* --> sempre errore se non variabile dopo contabilizzazione
110528130410    2c                   if        tasfic<>*blanks and §7lvdf=*blanks
110529130409     c                   Movel     Msg(11)       D66Msg
110530130409     c                   Eval      *In90 = *On
110531130410   x2c                   else
110532130410     c* BOLLA NON CONTABILIZZATA oppure VARIABILE DOPO CONTABILIZZ.
110533130410     c* Se variazione di tassazione
110534130415    3c                   if        %subst(vidcod:1:1) = 'T'
110535130410     c* Bolla     Contabilizzata: si può variare solo ias e vmd
110536130410    4c                   if        tasfic<>*blanks
110537130410     c                   eval      d66bdi='S'
110538130410   x4c                   else
110539130410     c* Bolla non Contabilizzata: controllo incasso e se già incassata
110540130410     c* si può variare solo IAS e VMD
110541130410    5c                   if        §7lbfi='S'
110542130410    6c                   if        arbica<>' ' and arbica <>'R'
110543130410    7c                   if        §7lvdf='S'
110544130409     c                   eval      d66bdi='S'
110545130410   x7c                   else
110546130410     c* errore se non variabile dopo incasso
110547130410     c                   Movel     Msg(17)       D66Msg
110548130410     c                   Eval      *In90 = *On
110549130410    7c                   endif
110550130410    6c                   endif
110551130410    6c                   if        arbica='R'
110552130409     C     KARI          CHAIN     FIARI01L
110553130410    7c                   if        %found(fiari01l) and ariatb=' '
110554130410    8c                   if        §7lvdf='S'
110555130409     c                   eval      d66bdi='S'
110556130410     c* errore se non variabile dopo incasso
110557130410     c                   Movel     Msg(17)       D66Msg
110558130410     c                   Eval      *In90 = *On
110559130410   x8c                   else
110560130410     c                   endif
110561130410    7c                   endif
110562130410    6c                   endif
110563130410    5c                   endif
110564130410    4c                   endif
110565130409    3c                   endif
110566130409    2c                   endif
110567130409    1c                   endif
110569130409     c                   endsr
110600000510**
110700941013Bolla inesistente                                                              1
110800941013Causale Variazione inesistente                                                 2
110900950331Causale Variazione non utilizzabile                                            3
111000941013Causale non utilizzabile: spedizione giacente.Variare eseguendo le disposizion 4
111100950131Causale Variazione per C/Assegno ma la bolla e' senza C/A o e' gia' incassato  5
111200941013Il codice bolla non puo' essere modificato per una spedizione locale           6
111300941013Causale non utilizzabile: Spedizione gia' consegnata                           7
111400941013Causale utilizzabile soltanto dopo la consegna completa o parziale, della bolla8
111500941013Causale non utilizzabile: e' gia' stata eseguita una consegna parziale         9
111600941013Per eseguire questo tipo di variazione tassare prima la bolla                 10
111700941013Causale non utilizzabile: spedizione gia' contabilizzata                      11
111800941013Causale utilizzabile soltanto dopo la contabilizzazione della bolla           12
111900941013Causale non utilizzabile per il tipo di bolla richiesto                       13
112000941013Causale utilizzabile soltanto per bolle Segue Fattura                         14
112100941013Causale utilizzabile soltanto per bolle con fattura immediata                 15
112200950120BOLLA LEGATA:il C/Assegno e' da variare sulla bolla MAMMA xx xxx xx xxxxxxx   16
112300950331Causale non utilizzabile: Spedizione gia' incassata                           17
112400981221Per i Cambi di Porto impossibile rimodificare il tipo bolla della bolla mamma
112500000510causale variazione non ammessa per bolle POSTE                                19
112600021128Causale non utilizzabile: spedizione con consegna particolare "SUPERMERCATI"  20
112700030127Causale non utilizzabile: spedizione senza bancali                            21
112800030127Causale non utilizzabile: variazione già effettuata                           22
112900061025Causale non utilizzabile dopo CONSEGNA per il tipo di bolla richiesto         23
113000061026Causale non utilizzabile dopo CONSEGNA PARZIALE per il tipo di bolla richiesto
113100070305Causale non utilizzabile: spedizione stornata (non viene fatturata)           25
