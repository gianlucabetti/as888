000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941012     H* FNLR66R *----------------------------------------------------*
000300941012     H*      VISUALIZZAZIONE E CONTROLLO CAUSALI VARIAZIONI BOLLE
000400130405      *
000500130405      * PER COMPILARE E' NECESSARIO AVERE IN LISTA LA GAITRAGRPS (per TITAS)
000600000000     H*--------------------------------------------------------------*
000700001213     FFNLR66D   CF   E             WORKSTN USROPN
000800941012     F                                     SFILE(LR66DSF:NRR)
000900950120     FFNLBL01L  IF   E           K DISK
001000950120     FTABEL00F  IF   E           K DISK
001100941012     FFNBLP01L  IF   E           K DISK
001200000510     FAZORG01L  IF   E           K DISK
001300941012     FFNARB01L  IF   E           K DISK
001400991011     FFIAR601L  IF   E           K DISK
001500050331     FTigcp21L  IF   E           K DISK    RENAME(TIGCP000:TIGCP21)
001600050331     FTigcp01L  IF   E           K DISK
001700030124     fFiar501l  if   e           k Disk
001800130409     fFiari01l  if   e           k Disk
001900130405     fTitas30c  if   e           k Disk    extfile(wlibsede) UsrOpn
002000990715     D*--------------------------------------------------------------*
002100151218     D MSG             S             78    DIM(27) CTDATA PERRCD(1)             MSG DI ERRORE
002200950120     D K78             S              1    DIM(78)                              COMODO X MSG
002300021128
002400030124     d kAr5Trd         s                   Like(Ar5Trd) inz('BAN')
002500021128     d Wtc1            s                   Like(ArbTc1)
002600021128     d Wtc2            s                   Like(ArbTc2)
002700060301     d Wcbo            s                   Like(Arbcbo)
002800061026     d Wdata           s                   Like(Arbdcm)
002900060302     D nullPtr         S               *
003000060302     D prmAas          S
003100060302     D                                     LIKE(diz$Aas)
003200060302     D prmLnp          S
003300060302     D                                     LIKE(diz$Lnp)
003400060302     D prmNrs          S
003500060302     D                                     LIKE(diz$Nrs)
003600060302     D prmNsp          S
003700060302     D                                     LIKE(diz$Nsp)
003800060302     D prmFiv          S
003900060302     D                                     LIKE(diz$Fiv)
004000060302     D prmDft          S
004100060302     D                                     LIKE(diz$Dft)
004200060302     D prmNft          S
004300060302     D                                     LIKE(diz$Nft)
004400060302     D prmEsito        S             10I 0
004500061026     D flag            S              1
004600061026     D tipo            S              1
004700061026     D des28           S             28
004800130405
004900130405     D Wlibsede        S             21
005000130405     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS30C')
005100130405     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS30C')
005200130409     d Wtip            s                   Like(Aritip) inz('A')
005300130409     d ktbl            s                   Like(tastbl)
005400130418     d ktbl1           s                   Like(tastbl)
005500151218     d wft1            s              1
005600151218     d wbopar          s              1
005700021128
005800941013     D***
005900941013     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006000941013     D***
006100941013     D WLBDAT          DS                  INZ
006200941013     D  G02DAT                 1      8  0
006300941013     D  G02INV                 9     16  0
006400941013     D  G02ERR                17     17
006500941013     D  G02TGI                18     22  0
006600000510     D OG143         E DS
006700000510     D DS7L          E DS
006800941013     D DS3A          E DS
006900920323     D KPJBA         E DS
007000941012     D*
007100941012     D* PASSAGGIO DATI
007200941012     D DSLR66        E DS                  EXTNAME(FNLR66DS)
007300941012     D*
007400941012     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
007500941012     D  DTU                  848    895P 0
007600941012     D                                     DIM(12)                              DATE PARAM
007700941012     D                                     PACKEVEN
007800030124     d dAr5Ban       e ds
007900061025     d ds7Lcampo     e ds
008000060302     D cndizion      E DS
008100060302     D                                     BASED(nullPtr)
008200060302     D                                     PREFIX(diz)
008300000000     C*---------------------------------------------------------------*
008400941012     C* INDICATORI
008500941012     C*---------------------------------------------------------------*
008600950331     C*  1    - INDICA SE PASSATA E TROVATA LA BOLLA DA DS
008700941012     C* 30    - DI COMODO
008800941012     C* 35    - PULIZIA ED EMISSIONE SFL
008900941013     C* 90    - ERORRE GENERICO
009000941012     C*---------------------------------------------------------------*
009100000000     C     *ENTRY        PLIST
009200000000     C                   PARM                    KPJBA
009300941012     C                   MOVEL     KPJBU         DSLR66
009400941012     C*
009500000000     C                   Z-ADD     1             CODUT
009600000000     C                   CALL      'XPARUT'
009700000000     C                   PARM                    UTEDSE
009800941012     C                   MOVEL     RAGUT         VIDDSA
009900000104     C                   MOVEL     DTU(10)       W0020             2 0
010000000104     C                   Z-ADD     DTU(10)       WDTU10
010100000104     C     W0020         IFGE      60
010200000104     C                   MOVEL     19            WDTU10
010300000104     C                   ELSE
010400000104     C                   MOVEL     20            WDTU10            8 0
010500000104     C                   ENDIF
010600941012     C*---------------------------------------------------------------*
010700941012     C* TIPO LANCIO, D15TLA "C" -           CHIUSO CON LR
010800941012     C* TIPO LANCIO, D15TLA "L" - ELABORO E CHIUDO CON LR
010900941012     C* TIPO LANCIO, D15TLA " " - ELABORO E CHIUDO IN RETRN
011000941012    1C     D66TLA        IFNE      'C'
011100941214     C                   SETOFF                                       90
011200001213    2C     D66GES        IFNE      'C'
011300140506    2C     D66GES        andne     'K'
011400001213    3C     OPN66         IFEQ      '0'
011500001213     C                   OPEN      FNLR66D
011600001213     C                   MOVE      '1'           OPN66
011700001213    3C                   ENDIF
011800001213   X2C                   ELSE
011900001213    3C     OPN66         IFEQ      '1'
012000001213     C                   CLOSE     FNLR66D
012100001213     C                   MOVE      '0'           OPN66
012200001213    3C                   ENDIF
012300001213    2C                   ENDIF
012400941012     C*
012500941012     C* VISUALIZZAZIONE PER SCELTA CODICE VARIAZIONE:
012600941013    2C     D66GES        IFEQ      'V'
012700941012     C* CARICO LE CAUSALI
012800920323     C                   EXSR      CARCOD
012900910219     C*
013000941013     C                   WRITE     LR66D02
013100900509     C     FOR01         TAG
013200941013     C                   EXFMT     LR66DCT
013300910912     C** CMD12 - RITORNO
013400920323     C  NKL              DO
013500910806     C* ENTER
013600920323     C                   EXSR      CONTR
013700941013     C     D66CVB        IFEQ      '  '
013800910219     C                   GOTO      FOR01
013900941013     C                   ENDIF
014000941013     C*
014100941013     C                   ENDDO
014200941013     C* SE IN VISUALIZZAZIONE TROVATO QUALCHE ERRORE NON MI INTERESSA
014300941013     C                   CLEAR                   D66MSG
014400941013     C*
014500941013  X2 C                   ELSE
014600941013     C*
014700941013     C* CONTROLLO LA CAUSALE VARIAZIONE ANCHE CON BOLLA SE IMMESSA
014800941013     C                   MOVEL     '7L'          COD
014900941013     C                   MOVEL     D66CVB        KEY
015000941013     C     KTAB2         CHAIN     TABEL                              30
015100941013     C*
015200941013    3C     *IN30         IFEQ      *ON
015300941013     C     *IN30         OREQ      *OFF
015400941013     C     TBLFLG        ANDNE     *BLANKS
015500941013     C                   MOVEL     MSG(2)        D66MSG
015600941013     C                   SETON                                        90
015700941013    3C                   ENDIF
015800941013     C*
015900941013    3C     *IN90         IFEQ      *OFF
016000941013     C                   MOVEL     TBLUNI        DS7L
016100941013     C                   MOVEL     §7LDES        D66DES
016200941013     C                   MOVEL     §7LFFI        D66FFI
016300941013     C                   MOVEL     §7LC4V        D66C4V
016400941013     C                   MOVEL     §7LSTF        D66STF
016500060130     C                   MOVEL     §7LPOCL       D66POCL
016600941013     C                   MOVEL     §7LFSA        D66FSA
016700941013     C                   MOVEL     §7LRTA        D66RTA
016800941102     C                   MOVEL     §7LBSF        D66BSF
016900130409     C                   MOVEL     §7LBFI        D66BFI
017000130409     C****               MOVEL     §7LFIN        D66BDI
017100060130     C                   MOVEL     §7LMTV        D66MTV
017200130411     C                   MOVEL     §7Lpda        D66pda
017300151218     c                   clear                   d66utp
017400941013     C* CONTROLLO BOLLA
017500941013     C                   EXSR      CTRBOL
017600941013     c*
017700941013    4C     *IN90         IFEQ      *OFF
017800941013     C                   MOVEL     D66CVB        VIDCOD
017900941013     C                   EXSR      CTRCVB
018000941013    4C                   ENDIF
018100941013    3C                   ENDIF
018200941013     C*
018300941013    2C                   ENDIF
018400941013    1C                   ENDIF
018500910219     C*
018600941013     C     D66MSG        IFNE      *BLANKS
018700941013     C                   MOVEL     '1'           D66ERR
018800941013     C                   ENDIF
018900941013     C*
019000941013     C                   MOVEL     DSLR66        KPJBU
019100941013     C* CHIUSURA PGM
019200941013    1C     D66TLA        IFEQ      ' '
019300941013     C                   RETURN
019400941013     C                   ELSE
019500001213     C*
019600001213     C     OPN66         IFEQ      '1'
019700001213     C                   CLOSE     FNLR66D
019800001213     C                   ENDIF
019900130405
020000130405     c                   if        %open(titas30c)
020100130405     c                   close     titas30c
020200130405     c                   endif
020300001213     C*
020400941013     C                   SETON                                        LR
020500941013    1C                   ENDIF
020600941013     C*
020700941013     C* OPERAZIONI INIZIALI ------------------------------------------*
020800941013     C     *INZSR        BEGSR
020900941013     C     KTAB          KLIST
021000941013     C                   KFLD                    CODUT
021100941013     C                   KFLD                    COD
021200941013     C     KTAB2         KLIST
021300941013     C                   KFLD                    CODUT
021400941013     C                   KFLD                    COD
021500941013     C                   KFLD                    KEY
021600941013     C     KBOL          KLIST
021700941013     C                   KFLD                    D66AAS
021800941013     C                   KFLD                    D66LNP
021900941013     C                   KFLD                    D66NRS
022000941013     C                   KFLD                    D66NSP
022100991011     C     KBOL6         KLIST
022200991011     C                   KFLD                    D66AAS
022300991011     C                   KFLD                    D66LNP
022400991011     C                   KFLD                    D66NRS
022500991011     C                   KFLD                    D66NSP
022600991011     C                   KFLD                    WTRC
022700030124
022800030124     c     kAr5          Klist
022900030124     c                   Kfld                    D66Aas
023000030124     c                   Kfld                    D66Lnp
023100030124     c                   Kfld                    D66Nrs
023200030124     c                   Kfld                    D66Nsp
023300030124     c                   Kfld                    kAr5Trd
023400130405     C     Ktas          KLIST
023500130405     C                   KFLD                    D66AAS
023600130405     C                   KFLD                    D66LNP
023700130405     C                   KFLD                    D66NRS
023800130405     C                   KFLD                    D66NSP
023900130418     C                   KFLD                    ktbl1
024000130419     C     Ktas1         KLIST
024100130419     C                   KFLD                    D66AAS
024200130419     C                   KFLD                    D66LNP
024300130419     C                   KFLD                    D66NRS
024400130419     C                   KFLD                    D66NSP
024500130409     C     Kari          KLIST
024600130409     C                   KFLD                    D66AAS
024700130409     C                   KFLD                    D66LNP
024800130409     C                   KFLD                    D66NRS
024900130409     C                   KFLD                    D66NSP
025000130409     C                   KFLD                    wtip
025100941013     C* *LIKE DEFN
025200941013     C     *LIKE         DEFINE    TBLKEY        KEY
025300941013     C     *LIKE         DEFINE    TBLCOD        COD
025400941013     C     *LIKE         DEFINE    ARBLNA        WLNA
025500941013     C     *LIKE         DEFINE    ARBLNP        WLNP
025600941013     C     *LIKE         DEFINE    ARBDCM        WDCM
025700941013     C     *LIKE         DEFINE    ARBICC        WICC
025800941013     C     *LIKE         DEFINE    ARBDCP        WDCP
025900981221     C     *LIKE         DEFINE    ARBCCA        WCCA
026000991011     C     *LIKE         DEFINE    AR6TRC        WTRC
026100001213     C*
026200001213     C                   MOVE      '0'           OPN66             1
026300941013     C*
026400941013     C                   ENDSR
026500910205     C*
026600941012     C* CARICO LE CAUSALI VARIAIZONI ---------------------------------*
026700920323     C     CARCOD        BEGSR
026800941209     C                   CLEAR                   VIDSEL
026900941012     C*
027000941012     C                   SETON                                        35
027100941013     C                   WRITE     LR66DCT
027200941012     C                   SETOFF                                       35
027300941012     C*
027400910205     C                   Z-ADD     0             NRR               4 0
027500941013     C*
027600941013     C* CARICO I DATI BOLLA SE C'E'
027700941013     C                   EXSR      CTRBOL
027800941012     C*
027900910806     C                   SETOFF                                       30
028000920323     C                   MOVEL     '7L'          COD
028100920323     C     KTAB          SETLL     TABEL
028200920323     C     KTAB          READE     TABEL                                  30
028300910220     C*
028400941012    1C     *IN30         DOWEQ     *OFF
028500061031     c                   clear                   descon
028600061031     c                   clear                   desconp
028700920323     C*
028800941012     C* ESCLUDO GLI ANNULLATI
028900941012    2C     TBLFLG        IFEQ      ' '
029000920323     C                   MOVEL     TBLUNI        DS7L
029100941012     C                   MOVEL     TBLKEY        VIDCOD
029200941013     C                   MOVEL     §7LDES        DESCOD
029300941013     C                   SETOFF                                       90
029400941013     C* CONTROLLO CON BOLLA
029500950706     C                   MOVEL     VIDCOD        FLGCVB
029600941013     C                   EXSR      CTRCVB
029700941013     C*
029800941013    3C     *IN90         IFEQ      *OFF
029900941012     C*
030000941012     C* UTILIZZAZIONE IN PARTENZA
030100941013    4C     §7LUCP        IFNE      'N'
030200941012     C                   MOVEL     'SI'          VIDUCP
030300941012     C                   ELSE
030400941012     C                   MOVEL     '  '          VIDUCP
030500941012    4C                   ENDIF
030600941013     C*
030700941012     C* UTILIZZAZIONE IN ARRIVO
030800941013    4C     §7LUCV        IFNE      'N'
030900941012     C                   MOVEL     'SI'          VIDUCV
031000941012     C                   ELSE
031100941012     C                   MOVEL     '  '          VIDUCV
031200941012    4C                   ENDIF
031300060130     C* CONTROLLA ANCHE P.O. CLIENTE
031400060130    4C     §7LPOCL       IFEQ      'S'
031500060130     C                   MOVEL     'SI'          VIDPOCL
031600060130     C                   ELSE
031700060130     C                   MOVEL     '  '          VIDpocl
031800060130    4C                   ENDIF
031900060130     C
032000941013     C*
032100920323     C* VARIAZIONE DOPO CONSEGNA
032200061026     c                   movel     §7lvdco       ds7lcampo
032300061026    4C     §7Lvdopo      IFEQ      'S'
032400061026     C                   MOVEL     '  SI '       VIDVDC
032500061026     C                   ELSE
032600061026    5C     §7Lvdopo      IFEQ      'C'
032700061026     C                   MOVEL     ' SOLO'       VIDVDC
032800061026     C                   ELSE
032900061026     C                   MOVEL     '  NO '       VIDVDC
033000061026    5C                   END
033100061026    4C                   END
033200061026    4C**   §7LVDC        IFEQ      'S'
033300061026     C**                 MOVEL     ' SI '        VIDVDC
033400061026     C**                 ELSE
033500061026    5C**   §7LVDC        IFEQ      'C'
033600061026     C*+                 MOVEL     'SOLO'        VIDVDC
033700061026     C**                 ELSE
033800061026     C**                 MOVEL     ' NO '        VIDVDC
033900061026    5C**                 END
034000061026    4C**                 END
034100061026     c                   exsr      Decodi7LCAMPO
034200061026     c                   if        des28<>*blanks
034300061031     c
034400061031     c                   move      vidvdc        alfa4             4
034500061031     c                   if        %subst(vidvdc:2:1)=' '
034600061031     c                   movel     ' *'          vidvdc
034700061031     c                   else
034800061031     c                   movel     '*'           vidvdc
034900061031     c                   endif
035000061031     c
035100061031     c                   eval      descon='DopCON:'+alfa4
035200061031     c                   eval      descon=%trim(descon)+' '+des28
035300061026     c                   endif
035400941012     C*
035500941012     C* VARIAZIONE DOPO CONSEGNA PARZIALE
035600061026     c                   movel     §7lvdcop      ds7lcampo
035700061026    4C     §7Lvdopo      IFEQ      'S'
035800061031     C                   MOVEL     ' SI  '       VIDVDP
035900941012     C                   ELSE
036000061026    5C     §7Lvdopo      IFEQ      'C'
036100061031     C                   MOVEL     ' SOLO'       VIDVDP
036200941012     C                   ELSE
036300061031     C                   MOVEL     ' NO  '       VIDVDP
036400941013    5C                   END
036500941013    4C                   END
036600061026    4C**   §7LVDP        IFEQ      'S'
036700061026     C**                 MOVEL     ' SI '        VIDVDP
036800061026     C**                 ELSE
036900061026    5C**   §7LVDP        IFEQ      'C'
037000061026     C**                 MOVEL     'SOLO'        VIDVDP
037100061026     C**                 ELSE
037200061026     C**                 MOVEL     ' NO '        VIDVDP
037300061026    5C**                 END
037400061026    4C**                 END
037500061026     c**
037600061026     c                   exsr      Decodi7LCAMPO
037700061026     c
037800061026     c                   if        des28<>*blanks
037900061026     c                   movel     '*'           vidvdp
038000061026     c                   move      vidvdp        alfa4
038100061031     c                   eval      desconp='DopPARZ:'+alfa4+des28
038200061026     c                   endif
038300941012     C*
038400920323     C* VARIAZIONE DOPO CONTABILIZZAZIONE
038500941013    4C     §7LVDF        IFEQ      'S'
038600941014     C                   MOVEL     ' SI '        VIDVDF
038700920323     C                   ELSE
038800941013    5C     §7LVDF        IFEQ      'C'
038900920323     C                   MOVEL     'SOLO'        VIDVDF
039000920323     C                   ELSE
039100941014     C                   MOVEL     ' NO '        VIDVDF
039200941013    5C                   END
039300941013    4C                   END
039400941012     C*
039500941013     C* FRANCHI/ ASSEGNATI/ 2 BOLLA
039600941013     C                   CLEAR                   DESTBL
039700941013    4C     §7LVFA        IFEQ      'F'
039800060301     c                   select
039900060301     c                   when      §7lprp = 'S'
040000060301     c                   eval      destbl='FRANCO Prepag'
040100060301     c                   when      §7lprp = 'N'
040200060301     c                   eval      destbl='FRANCO NoPrep'
040300060301     c                   when      §7lprp = ' '
040400060301     c                   eval      destbl='FRANCO       '
040500060301     c*****              eval      destbl='FRANCO'
040600060301     c                   endsl
040700920323     C                   ELSE
040800941013    5C     §7LVFA        IFEQ      'A'
040900941013     C                   MOVEL     'ASSEG'       DESTBL
041000920323     C                   ELSE
041100941013    6C     §7LVFA        IFEQ      'E'
041200941013     C                   MOVEL     '2 BOL'       DESTBL
041300920323     C                   ELSE
041400941013     C                   MOVEL     'TUTTE'       DESTBL
041500941013    6C                   END
041600941013    5C                   END
041700941013    4C                   END
041800941013     C*
041900941013     C* VARIAZIONE PER BOLLE CON C/ASS
042000941013     C     §7LFCA        IFEQ      'S'
042100060126     c                   eval      destbl=%trim(destbl)+(' con C/A')
042200941013     C                   ENDIF
042300941013     C*
042400920323     C* PER BOLLA SEGUE FATTURA
042500941013    4C     §7LBSF        IFEQ      'S'
042600060126     c                   eval      destbl=%trim(destbl)+(' S.F.')
042700941013    4C                   END
042800920323     C* PER BOLLA FATTURA IMMEDIATA
042900941013    4C     §7LBFI        IFEQ      'S'
043000060126     c                   eval      destbl=%trim(destbl)+(' FT.IMM')
043100941013    4C                   END
043200060130     C* DA STORICIZZARE ora flag unico
043300941013    4C     §7LFSA        IFEQ      'S'
043400941012     C                   MOVEL     'SI'          VIDFSA
043500941012     C                   ELSE
043600941012     C                   MOVEL     '  '          VIDFSA
043700941013    4C                   END
043800060130     c
043900941012     C* DA STORICIZZARE ALLA PARTENZA
044000060130    4C**   §7LFSP        IFEQ      'S'
044100060130     C**                 MOVEL     'SI'          VIDFSP
044200060130     C**                 ELSE
044300060130     C**                 MOVEL     '  '          VIDFSP
044400060130    4C**                 END
044500060130     C* MEMORIZZAZIONE MOTIVO VARIAZIONE
044600060130    4C     §7LMTV        IFEQ      'S'
044700060130     C                   MOVEL     'SI'          VIDMTV
044800060130     C                   ELSE
044900060130     C                   MOVEL     '  '          VIDMTV
045000060130    4C                   END
045100910912     C*
045200941013     C                   MOVEL     §7LFFI        FLGFFI
045300941013     C                   MOVEL     §7LC4V        FLGC4V
045400941013     C                   MOVEL     §7LSTF        FLGSTF
045500060130     C                   MOVEL     §7LPOCL       FLGPOCL
045600941013     C                   MOVEL     §7LFSA        FLGFSA
045700941013     C                   MOVEL     §7LRTA        FLGRTA
045800060130     C                   MOVEL     §7LMTV        FLGMTV
045900910205     C* SCRIVO SUBFILE
046000910205     C                   ADD       1             NRR
046100941013     C                   WRITE     LR66DSF
046200941013    3C                   END
046300941013    2C                   END
046400910912     C*
046500920323     C     KTAB          READE     TABEL                                  30
046600941013    1C                   ENDDO
046700910205     C*
046800910205     C                   ENDSR
046900941013     C*
047000941013     C* CARICO LA BOLLA ----------------------------------------------*
047100941013     C     CTRBOL        BEGSR
047200941013     C                   SETOFF                                       01
047300941217     C                   CLEAR                   WGIAC
047400061026     C                   CLEAR                   ds3a
047500061026     C                   CLEAR                   Wfineksc1
047600061026     C                   CLEAR                   Wfineksc2
047700151218     C                   CLEAR                   Wft1
047800151218     C                   CLEAR                   Wbopar
047900941013     C*
048000941013     C* SE IMMESSA LA BOLLA FACCIO CONTROLLO INCROCIATI
048100941013    1C     D66NSP        IFGT      0
048200941013     C* BOLLA ARRIVI
048300941013    2C     D66TBO        IFEQ      'A'
048400941013     C     KBOL          CHAIN     FNARB000                           30
048500941013     C*
048600941013    3C     *IN30         IFEQ      *OFF
048700941013     C                   SETON                                        01
048800060301     c                   movel     arbcbo        wcbo
048900991011     C                   MOVEL     '1'           WTRC
049000991011     C     KBOL6         CHAIN     FIAR6000                           30
049100941013     C  N30              MOVE      AR6DFT        WDFT                           D.FATT 1 BOL
049200941013     C   30              CLEAR                   WDFT
049300061026     c* codice segue fattura/fattura immediata
049400061026     c  n30              move      ar6ksc        wfineksc1         4 0
049500061026     c   30              move      arbksc        wfineksc1
049600941013     C                   MOVEL     ARBDCM        WDCM                           D.CONSEGNA
049700941013     C                   MOVEL     ARBDCP        WDCP                           D.CONS.PARZ
049800941013     C                   MOVEL     ARBLNP        WLNP                           LIN PARTENZA
049900941013     C                   MOVEL     ARBLNA        WLNA                           LIN ARRIVO
050000941013     C                   MOVEL     ARBICC        WICC                           INCASSO C/A
050100981221     C                   MOVEL     ARBCCA        WCCA                           CONS.ANOMALA
050200021128      * Imposto flag consegne particolari
050300021128     c                   Eval      Wtc1 = ArbTc1
050400021128     c                   Eval      Wtc2 = ArbTc2
050500021128
050600941013     C*
050700941013     C* DATA FATTURA: SE 2 BOLLA LA PRENDO DALLA ESTESIONE
050800941013     C                   MOVEL     '3A'          COD                            COD BOLLA
050900941013     C                   MOVEL(P)  ARBCBO        KEY
051000941013     C     KTAB2         CHAIN     TABEL                              31
051100941013     C  N31              MOVEL     TBLUNI        DS3A
051200941013     C*
051300941013     C* VEDO SE GIACENTE
051400050331     C     KBOL          CHAIN     Tigcp21L                           31        GIACENZA
051500941013    4C     *IN31         IFEQ      *OFF
051600050331     C     GCPFAS        ANDLT     40
051700941013     C     GCPATB        ANDEQ     ' '
051800941013     C                   MOVEL     'S'           WGIAC             1
051900941013    4C                   ENDIF
052000941013     C*
052100941013     C*GIRO DATA SPEDIZIONE
052200941013     C                   MOVEL     '3'           G02ERR
052300941013     C                   Z-ADD     ARBMGS        G02INV
052400941013     C                   MOVEL     ARBAAS        G02INV
052500941013     C                   CALL      'XSRDA8'
052600941013     C                   PARM                    WLBDAT
052700941013     C*
052800941013     C                   Z-ADD     G02DAT        VIDDSP
052900941013    3C                   ENDIF
053000151218     c*Se non trovata la bolla in arrivo e il chiamante ammette la manutenzione di
053100151218     c* bolle non ancora partite
053200160115     c* Per CVB che utilizza anche il p.o. cliente non permetto di andare su
053300160115     c* BLP perchè le variazioni di tassazione assegnati non le permettiamo su
053400160115     c* bolle ancora da partire
053500160115     c                   if        not *in01 and d66bnp='S' and d66pocl=*blanks
053600151218     c                   exsr      sr_dablp
053700151218     c                   if        *in01
053800151218     c                   eval      wbopar='S'
053900151218     c                   eval      wft1=blpft1
054000151218     c                   endif
054100151218     c                   endif
054200941013     C*
054300941013   X2C                   ELSE
054400151218     C* BOLLA PARTENZA
054500151218     c                   exsr      sr_dablp
054600941013     C*
054700941013    2C                   ENDIF
054800061026     c
054900061026     c* Chain su seconda bolla, se presente
055000061026    4C     §3ATB2        IFNE      *BLANKS
055100061026     C                   MOVEL     '2'           WTRC
055200061026     C     KBOL6         CHAIN     FIAR6000                           32
055300061026     C  N32              MOVE      AR6DFT        W2DFT             8 0          D.FATT 2 BOL
055400061026     C   32              CLEAR                   W2DFT
055500061026     c  n32              move      ar6ksc        wfineksc2         4 0
055600061026    4C                   ENDIF
055700941013     C*
055800941013     C* NON TROVATA BOLLA
055900941013    2C     *IN01         IFEQ      *OFF
056000060301     c                   clear                   wcbo
056100941013     C                   MOVEL     MSG(1)        D66MSG
056200941013     C                   SETON                                        90
056300941013   X2C                   ELSE
056400941013     C*
056500941013     C* IMPOSTO I FLAG CHE MI SERVONO PER I CONTROLLI
056600941013     C                   MOVEL     VIDCOD        FLGCVB            1
056700941013     C                   MOVEL     §3ATB1        FLGTB1            1
056800941013    2C                   ENDIF
056900030124
057000060301     c* se bolla avente codice "M"=C/S per prepagati annullati
057100060301     c* non è possibile effettuare alcuna variazione
057200060301     c                   if        wcbo = 'M '
057300060301     C                   MOVEL     MSG(13)       D66MSG
057400060301     C                   SETON                                        90
057500060301     c                   endif
057600030124      * Se richiesta causale x Bancali la bolla deve avere il record 'BAN'
057700030124      * in Fira5 e non deve essere già stato mod.
057800030124     c                   If        D66Cvb = 'BR'
057900030124     c                   Clear                   dAr5Ban
058000030124     c     Kar5          Chain     Fiar501l
058100030124     c                   If        Not %Found(Fiar501l)
058200030127     c                   Movel     Msg(21)       D66Msg
058300030124     c                   Eval      *In90 = *On
058400030124     c                   Else
058500030124     c                   Movel     Ar5Uni        dAr5Ban
058600030124     c                   If        §Ar5Rb1 > *Zeros or §Ar5Rb2 > *Zeros
058700030127     c                   Movel     Msg(22)       D66Msg
058800030124     c                   Eval      *In90 = *On
058900030124     c                   EndIf
059000030124     c                   EndIf
059100030124     c                   EndIf
059200941013     C*
059300941013    1C                   ENDIF
059400920323     C*
059500941013     C                   ENDSR
059600151218     C* Bolla da blp -------------------------------------------------*
059700151218     C     sr_dablp      BEGSR
059800151218     C* BOLLA PARTENZA
059900151218     C     KBOL          CHAIN     FNBLP000                           30
060000151218     C*
060100151218    3C     *IN30         IFEQ      *OFF
060200151218     C                   SETON                                        01
060300151218     c                   movel     blpcbo        wcbo
060400151218     C                   MOVEL     '1'           WTRC
060500151218     C     KBOL6         CHAIN     FIAR6000                           30
060600151218     C  N30              MOVE      AR6DFT        WDFT              8 0          D.FATT 1 BOL
060700151218     C   30              CLEAR                   WDFT
060800151218     c* codice segue fattura/fattura immediata
060900151218     c  n30              move      ar6ksc        wfineksc1         4 0
061000151218     c   30              move      arbksc        wfineksc1
061100151218     C                   MOVEL     BLPDCM        WDCM                           D.CONSEGNA
061200151218     C                   MOVEL     BLPDCP        WDCP                           D.CONS.PARZ
061300151218     C                   MOVEL     BLPLNP        WLNP                           LIN PARTENZA
061400151218     C                   MOVEL     BLPLNA        WLNA                           LIN ARRIVO
061500151218     C                   MOVEL     BLPCCA        WCCA                           CONS.ANOMALA
061600151218      * Imposto flag consegne particolari
061700151218     c                   Eval      Wtc1 = BlpTc1
061800151218     c                   Eval      Wtc2 = BlpTc2
061900151218
062000151218     C                   CLEAR                   W2DFT                          D.FATT 2 BOL
062100151218     C* COME SE FOSSE SEMPRE INCASSATO: NON SI PUO' VARIARE
062200151218     C                   MOVEL     'S'           WICC                           INCASSO C/A
062300151218     C* DATA FATTURA: SE 2 BOLLA LA PRENDO DALLA ESTESIONE
062400151218     C                   MOVEL     '3A'          COD
062500151218     C                   MOVEL(P)  BLPCBO        KEY
062600151218     C     KTAB2         CHAIN     TABEL                              31
062700151218     C  N31              MOVEL     TBLUNI        DS3A
062800151218     C*
062900151218     C* VEDO SE GIACENTE
063000151218     C     KBOL          CHAIN     Tigcp01L                           31
063100151218    4C     *IN31         IFEQ      *OFF
063200151218     C     GCPFAS        ANDLT     60
063300151218     C     GCPATB        ANDEQ     ' '
063400151218     C                   MOVEL     'S'           WGIAC             1
063500151218    4C                   ENDIF
063600151218     C*
063700151218     C*GIRO DATA SPEDIZIONE
063800151218     C                   MOVEL     '3'           G02ERR
063900151218     C                   Z-ADD     BLPMGS        G02INV
064000151218     C                   MOVEL     BLPAAS        G02INV
064100151218     C                   CALL      'XSRDA8'
064200151218     C                   PARM                    WLBDAT
064300151218     C*
064400151218     C                   Z-ADD     G02DAT        VIDDSP
064500151218    3C                   ENDIF
064600151218     c                   endsr
064700941013     C*
064800941013     C* CONTROLLO LA CAUSALE CON LA BOLLA IMMESSA --------------------*
064900941013     C     CTRCVB        BEGSR
065000941013     C*
065100941013     C                   SETOFF                                       90
065200000510     C*
065300000510     C     WLNP          CHAIN     AZORG01L                           30
065400000510     C  N30              MOVEL     ORGDE3        OG143
065500000510     C   30              CLEAR                   OG143
065600941013     C*
065700140502     c* i controlli seguenti li ignoro per le causali utilizzate solo
065800140502     c* per chiamate cieche (entrambi i codici UCP e UCV='N')
065900140506     c* per dare la possibilità al chiamante di richiamare LR66R per
066000140502     c* esecuzione dei controlli
066100140506     c                   if        d66ges<>'K' or §7lucv<>'N' or §7lucp<>'N'
066200941013     C* ESCLUDO LE BOLLE DA NON UTILIZZARE SE SO IL TIPO DI BOLLA
066300941013    1C     D66TBO        IFEQ      'A'
066400151217     c                   select
066500151218     c*- - - - - - - - - - - - - - - -
066600151218     c* cvb NON utilizzabile in arrivo
066700151218     c*       OPPURE
066800151218     c* cvb utilizzabile in arrivo ma solo su bolle in partenza ma pgm non abilitato
066900151218     c*- - - - - - - - - - - - - - - -
067000151218     C                   when      §7lucv='N' or
067100151218     C                             (d66bnp=*blanks and §7lucv='P')
067200941013     C                   SETON                                        90
067300941013     C                   MOVEL     MSG(3)        D66MSG
067400151218     c*- - - - - - - - - - - - - - - -
067500151218     c* cvb utilizzabile in arrivo ma SOLO su BLP NON TRASMESSO
067600151218     c* --> errore se bolla già in ARB oppure in BLP già trasmessa
067700151218     c*- - - - - - - - - - - - - - - -
067800151218     C                   when      *in01 and §7lUCV='P' and
067900151218     c                             (wbopar=*blanks or wft1<>*blanks)
068000151218     C                   SETON                                        90
068100151218     c                   if        wbopar='S' and wft1='N' and wcca='7'
068200151218     C                   MOVEL     MSG(27)       D66MSG
068300151218     c                   else
068400151218     C                   MOVEL     MSG(3)        D66MSG
068500151218     c                   eval      d66msg=%trim(d66msg)
068600151218     c                             + ' per bolla già in arrivo o consegnata'
068700151218     c                   endif
068800151218     c*- - - - - - - - - - - - - - - -
068900151218     c* cvb utilizzabile in arrivo SOLO su ARB
069000151218     c* --> errore: Causale Variazione utilizzabile solo per bolle presenti in arrivo
069100151218     c*- - - - - - - - - - - - - - - -
069200151218     C                   when      *in01 and §7lUCV=' ' and wbopar='S'
069300151218     C                   SETON                                        90
069400151218     C                   MOVEL     MSG(19)       D66MSG
069500151218     c*- - - - - - - - - - - - - - - -
069600151218     c* cvb utilizzabile in arrivo su ARB Oppure su BLP NON TRASMESSO
069700151218     c* --> errore se bolla BLP già trasmesso
069800151218     c*- - - - - - - - - - - - - - - -
069900151218     C                   when      *in01 and
070000151218     C                             §7lUCV='T' and wbopar='S' and wft1<>*blanks
070100151218     C                   SETON                                        90
070200151218     c* Bolla chiusa in partenza per merce mai affidata
070300151218     c                   if        wft1='N' and wcca='7'
070400151218     C                   MOVEL     MSG(27)       D66MSG
070500151218     c                   else
070600151218     c* bolla non manutenzionabile
070700151218     C                   MOVEL     MSG(26)       D66MSG
070800151218     c                   endif
070900151217     c                   endsl
071000941013    1C                   ENDIF
071100941013     C*
071200941013    1C     D66TBO        IFEQ      'P'
071300941014     C     §7LUCP        ANDEQ     'N'
071400941013     C                   SETON                                        90
071500941013     C                   MOVEL     MSG(3)        D66MSG
071600941013    1C                   ENDIF
071700140502
071800140502     c                   endif
071900941013     C*
072000160119     c                   if        §7lucv='T' or §7lucv='P'
072100151221     c                   eval      d66utp=§7lucv
072200151221     c                   endif
072300160119     c* se causale utilizzabile solo per chiamate cieche abilito alla
072400160119     c* manutenzione di una bolla ancora da partire
072500160119     c                   if        §7lucv='N' and §7lucp='N'
072600160119     c                   eval      d66utp='T'
072700160119     c                   endif
072800151221
072900941013    1C     *IN01         IFEQ      *ON
073000941013     C     *IN90         ANDEQ     *OFF
073100151218     c*
073200941013     C*
073300941013     C* SE GIACENTE ESCLUDO LA VARIAZIONE DI C/A
073400011121      * PER LE BOLLE ARRIVI
073500941013    2C     WGIAC         IFEQ      'S'
073600011121     C     D66TBO        ANDEQ     'A'
073700941013     C     FLGCVB        ANDEQ     'K'
073800011120     C     VIDCOD        ANDNE     'KA'
073900011120     C     VIDCOD        ANDNE     'KP'
074000941013     C                   SETON                                        90
074100941013     C                   MOVEL     MSG(4)        D66MSG
074200941013     C                   GOTO      ENDCCV
074300941013    2C                   ENDIF
074400011121      * PER LE BOLLE PARTENZA
074500011121    2C     WGIAC         IFEQ      'S'
074600011121     C     D66TBO        ANDEQ     'P'
074700011121     C     FLGCVB        ANDEQ     'K'
074800011121     C                   SETON                                        90
074900011121     C                   MOVEL     MSG(4)        D66MSG
075000011121     C                   GOTO      ENDCCV
075100011121    2C                   ENDIF
075200941013     C**
075300941013     C* F L A G   C / A S S E G N O
075400941013     C**
075500941013     C* SE NON C'E' C/A  O GIA' INCASSATO NON POSSO USARE LA KA/KB
075600941013    2C     §7LFCA        IFEQ      'S'
075700941013    3C     §3AFCA        IFEQ      0
075800950119     C     WICC          ORNE      ' '
075900950119     C     WICC          ANDNE     'R'
076000941013     C                   SETON                                        90
076100941013     C                   MOVEL     MSG(5)        D66MSG
076200941013     C                   GOTO      ENDCCV
076300941013    3C                   ENDIF
076400950120     C* SE C'E IL FLAG C/A MA E' BOLLA LEGATA IN LOCALE --> DEVO
076500950120     C*  VARIARE LA MAMMA
076600950120    3C     §3AFCA        IFNE      0
076700950120     C     KBOL          CHAIN     FNLBL01L                           30
076800950120    4C     *IN30         IFEQ      *OFF
076900950120     C     LBLLAP        ANDEQ     LBLLAN
077000950120     C                   SETON                                        90
077100950120     C* PREPARO IL MESSAGGIO
077200950120     C                   MOVEA     MSG(16)       K78
077300950120     C                   MOVE      LBLAAP        W002A             2
077400950120     C                   MOVEA     W002A         K78(59)
077500950120     C                   MOVEL     LBLLPP        W003A             3
077600950120     C                   MOVEA     W003A         K78(62)
077700950120     C                   MOVE      LBLNRP        W002A
077800950120     C                   MOVEA     W002A         K78(66)
077900950120     C                   MOVE      LBLNSP        W007A             7
078000950120     C                   MOVEA     W007A         K78(69)
078100950120     C*
078200950120     C                   MOVEA     K78           D66MSG
078300950120     C                   GOTO      ENDCCV
078400950120    4C                   ENDIF
078500950120    3C                   ENDIF
078600950120    2C                   ENDIF
078700000510     C* PER BOLLE POSTE
078800151217     C**   §OGPT         IFEQ      'S'
078900151217     C**   §7LFPT        ANDEQ     ' '
079000151217     C**                 SETON                                        90
079100151217     C**                 MOVEL     MSG(19)       D66MSG
079200151217     C**                 GOTO      ENDCCV
079300151217    3C**                 ENDIF
079400941013     C**
079500941013     C* T I P O   D I   B O L L A
079600941013     C**
079700941013    2C     §7LVFA        IFEQ      'F'
079800060301     C***  FLGTB1        ANDNE     'F'
079900060301     c                   if        flgtb1 <> 'F'  or
080000060301     c                             (§7lprp =  'S'  and wdft = 0 ) or
080100060301     c                             (§7lprp =  'N'  and wdft > 0 )
080200941013     C                   SETON                                        90
080300060301     c                   endif
080400941013    2C                   ENDIF
080500941013    2C     §7LVFA        IFEQ      'A'
080600941013     C     FLGTB1        ANDNE     'A'
080700941013     C                   SETON                                        90
080800941013    2C                   ENDIF
080900941013    2C     §7LVFA        IFEQ      'E'
081000941013     C     §3ATB2        ANDEQ     *BLANKS
081100941013     C                   SETON                                        90
081200941013    2C                   ENDIF
081300941013     C*
081400941013    2C     *IN90         IFEQ      *ON
081500061025     C                   MOVEL     MSG(13)       D66MSG
081600941013     C                   GOTO      ENDCCV
081700941013    2C                   ENDIF
081800941013     C**
081900941013     C* C A M B I O   C O D I C E   B O L L A
082000941013     C**
082100941013     C* PER TIPI BOLLA CREATI IN AUTOMATICO NON E' POSSIBILE
082200941013     C* SE E' DIROTTAMENTO AD ALTRA FILIALE, SI
082300941013    2C     VIDCOD        IFEQ      'K8'
082400941013     C     VIDCOD        OREQ      'K5'
082500941013     C*
082600941013    3C     §3AUCB        IFEQ      'N'
082700941013     C     WLNP          ANDEQ     WLNA
082800941013     C                   SETON                                        90
082900941013     C                   MOVEL     MSG(6)        D66MSG
083000941013     C                   GOTO      ENDCCV
083100941013    3C                   ENDIF
083200981221     C* SE E' CAMBIO DI PORTO NON POSSO MODIFICARE IL TIPO BOLLA
083300981221    3C     §3AUCB        IFEQ      'N'
083400981221     C     WCCA          ANDEQ     '9'
083500981221     C                   SETON                                        90
083600981221     C                   MOVEL     MSG(18)       D66MSG
083700981221     C                   GOTO      ENDCCV
083800981221    3C                   ENDIF
083900941013    2C                   ENDIF
084000941013     C**
084100941013     C* F L A G   C O N S E G N A
084200941013     C**
084300061025     c* Metto il flag in DS
084400061026     c                   movel     §7lvdco       ds7lcampo
084500061026     c* Imposto il camp data con la data consegna
084600061026     c                   eval      Wdata=Wdcm
084700061026     c                   Exsr      CTRds7lcampo
084800061026     c
084900061026     c                   if        tipo='P' and flag='1'
085000061026     C                   MOVEL     MSG(7)        D66MSG
085100061026     C                   GOTO      ENDCCV
085200061026    2C                   ENDIF
085300061026     c                   if        tipo='D' and flag='1'
085400061026     C                   MOVEL     MSG(8)        D66MSG
085500061026     C                   GOTO      ENDCCV
085600061026    2C                   ENDIF
085700061026     c                   if        tipo='D' and flag='2'
085800061026     C                   MOVEL     MSG(23)       D66MSG
085900061026     C                   GOTO      ENDCCV
086000061026    2C                   ENDIF
086100061026     c
086200941013     C* NON VARIABILE DOPO CONSEGNA
086300061025    2C***  §7LVDC        IFEQ      ' '
086400061025     C***  §7LVDC        OREQ      'N'
086500061025    3C***  WDCM          IFGT      0
086600061025     C***                SETON                                        90
086700061025     C***                MOVEL     MSG(7)        D66MSG
086800061025     C***                GOTO      ENDCCV
086900061025    3C***                ENDIF
087000061025    2C***                ENDIF
087100061025     C***
087200941013     C* VARIABILE SOLO DOPO LA CONSEGNA
087300061025    2C***  §7LVDC        IFEQ      'C'
087400061025     C***  WDCM          ANDEQ     0
087500061025     C***                SETON                                        90
087600061025     C***                MOVEL     MSG(8)        D66MSG
087700061025     C***                GOTO      ENDCCV
087800061025    2C***                ENDIF
087900941013     C**
088000941013     C* F L A G   C O N S E G N A   P A R Z I A L E
088100941013     C**
088200061026     C**
088300061102     c* lo controllo solo se la data consegna = 0 altrimenti non lo faccio
088400061102     c                   if        wdcm=0
088500061026     c* Metto il flag in DS
088600061026     c                   movel     §7lvdcop      ds7lcampo
088700061026     c* Imposto il camp data con la data consegna
088800061026     c                   eval      Wdata=Wdcp
088900061026     c                   Exsr      CTRds7lcampo
089000061026     c
089100061026     c                   if        tipo='P' and flag='1'
089200061026     C                   MOVEL     MSG(9)        D66MSG
089300061026     C                   GOTO      ENDCCV
089400061026    2C                   ENDIF
089500061026     c                   if        tipo='D' and flag='1'
089600061026     C                   MOVEL     MSG(8)        D66MSG
089700061026     C                   GOTO      ENDCCV
089800061026    2C                   ENDIF
089900061026     c                   if        tipo='D' and flag='2'
090000061026     C                   MOVEL     MSG(24)       D66MSG
090100061026     C                   GOTO      ENDCCV
090200061026    2C                   ENDIF
090300061102    2C                   ENDIF
090400941013     C* NON VARIABILE DOPO CONSEGNA PARZIALE
090500061026    2C**   §7LVDP        IFEQ      ' '
090600061026     C**   WDCP          ANDGT     0
090700061026     C**                 SETON                                        90
090800061026     C**                 MOVEL     MSG(9)        D66MSG
090900061026     C**                 GOTO      ENDCCV
091000061026    2C**                 ENDIF
091100941013     C*
091200941013     C* VARIABILE SOLO DOPO LA CONSEGNA PARZIALE
091300061026    2C**   §7LVDP        IFEQ      'C'
091400061026     C**   WDCP          ANDEQ     0
091500061026     C**                 SETON                                        90
091600061026     C**                 MOVEL     MSG(8)        D66MSG
091700061026     C**                 GOTO      ENDCCV
091800061026    2C**                 ENDIF
091900941013     C**
092000941013     C* F L A G   C O N T A B I L I Z Z A Z I O N E
092100941013     C**
092200941013     C* PRENDO LA DATA FATTURA DELLA 2 BOLLE SE
092300941013     C*  1) CAUSALE INIZIA PER 'K'
092400941013     C*  2) SE E' PROPRIO PER 2 BOLLA
092500941013     C*  3) SE E' VARIAZIONE DEL DESTINATARIO
092600941013    2C     §7LVFA        IFEQ      'E'
092700941013     C*
092800941013     C     §3ATB2        ORNE      *BLANKS
092900941013     C     FLGCVB        ANDEQ     'K'
093000941013     C*
093100950301     C     §3ATB2        ORNE      *BLANKS
093200950301     C     VIDCOD        ANDEQ     'I0'
093300000104     C                   MOVEL     W2DFT         WDFTC             8 0
093400941013   X2C                   ELSE
093500941013     C                   MOVEL     WDFT          WDFTC
093600941013    2C                   ENDIF
093700941013     C*
093800941013     C* NON VARIABILE DOPO LA CONTABILIZZAZIONE
093900941013    2C     WDFTC         IFGT      0
094000941013     C     §7LVDF        ANDEQ     ' '
094100000104     C     WDFTC         ANDLE     WDTU10
094200941013     C                   SETON                                        90
094300941013     C                   MOVEL     MSG(11)       D66MSG
094400941013     C                   GOTO      ENDCCV
094500941013    2C                   ENDIF
094600941013     C*
094700941013     C* VARIABILE SOLO DOPO LA CONTABILIZZAZIONE
094800941013    2C     §7LVDF        IFEQ      'C'
094900000104    3C     WDFTC         IFGT      WDTU10
095000941013     C     WDFTC         OREQ      0
095100941013     C                   SETON                                        90
095200941013     C                   MOVEL     MSG(12)       D66MSG
095300941013     C                   GOTO      ENDCCV
095400941013    3C                   ENDIF
095500941013    2C                   ENDIF
095600130410     c* Non variabile dopo contabilizzazione/incasso per fatture immediate
095700130405     c                   exsr      opnfile
095800130614     c***                if        §7lbfi='S' or §7lvdf=' '
095900130410     c* Setll su Titas senza tipo bolla e se bolla non esiste
096000130410     c* sicuramente non è contabilizzata e quindi permetto la variazione
096100130409     c     kbol          setll     titas30c
096200130409     c                   if        %equal(titas30c)
096300130409     c                   clear                   ktbl
096400130409     c                   select
096500130409     c* bolla variabile: franco o assegnato chaino con tipo bolla §3atb1
096600130409     c                   when      §7lvfa='F' or §7lvfa='A'
096700130405     c                   eval      ktbl=§3atb1
096800130409     c                   exsr      chtitas
096900130409     c   90              goto      endccv
097000130409     c* bolla variabile: estensione chaino con tipo bolla §3atb2
097100130409     c                   when      §7lvfa='E'
097200130409     c                   eval      ktbl=§3atb2
097300130409     c                   exsr      chtitas
097400130409     c   90              goto      endccv
097500130409     c                   other
097600130409     c* bolla variabile: tutte chaino con §3atb1 e §3atb2 se presente
097700130409     c                   eval      ktbl=§3atb1
097800130409     c                   exsr      chtitas
097900130409     c   90              goto      endccv
098000130409     c                   if        §3atb2<>*blanks
098100130409     c                   eval      ktbl=§3atb2
098200130409     c                   exsr      chtitas
098300130409     c   90              goto      endccv
098400130409     c                   endif
098500130409     c                   endsl
098600130409     c
098700130409     c                   endif
098800130614     c***                endif
098900941013     C**
099000941013     C* P E R   B O L L A     S E G U E   F A T T U R A
099100941013     C**
099200941013     C     §7LBSF        IFEQ      'S'
099300941013     C     WDFTC         ANDGT     0
099400941013     C                   SETON                                        90
099500941013     C                   MOVEL     MSG(14)       D66MSG
099600941013     C                   GOTO      ENDCCV
099700941013     C                   ENDIF
099800941013     C**
099900941013     C* P E R   B O L L A   F A T T U R A   I M M E D I A T A
100000941013     C**
100100941013     C     §7LBFI        IFEQ      'S'
100200941013     C     WDFTC         ANDEQ     0
100300941013     C                   SETON                                        90
100400941013     C                   MOVEL     MSG(15)       D66MSG
100500941013     C                   GOTO      ENDCCV
100600941013     C                   ENDIF
100700070305     c
100800070305     c* Non faccio utilizzare se cpdioce bollo storno assegnato
100900070305     c                   if        §7lBFI='S' or §7lBSF='S'
101000070305     c                   if        (§7lvfa='A' and §3atb1='AS') or
101100070305     c                             (§7lvfa='E' and §3atb2='AP')
101200070305     C                   SETON                                        90
101300070305     C                   MOVEL     MSG(25)       D66MSG
101400070305     C                   GOTO      ENDCCV
101500070305     C                   ENDIF
101600070305     c                   endif
101700070305     c
101800950331     C* SE GIA INCASSATA NON SI PUO' UTILIZZARE LA CAUSALE PER
101900990415     C* FATTURE IMMEDIATE SE LA CAUASALE NON LO PERMETTE
102000130410     C***  §7LBFI        IFEQ      'S'
102100130410     C***  §7LFIN        ANDNE     'S'
102200130410     C***  ARBICA        ifne      ' '
102300130410     C***  ARBICA        ANDNE     'R'
102400130410     C***                SETON                                        90
102500130410     C***                MOVEL     MSG(17)       D66MSG
102600130410     C***                GOTO      ENDCCV
102700130410     c***                endif
102800130410     c***  arbica        ifeq      'R'
102900130410     C***  KARI          CHAIN     FIARI01L
103000130410     c***                if        %found(fiari01l) and ariatb=' '
103100130410     C***                SETON                                        90
103200130410     C***                MOVEL     MSG(17)       D66MSG
103300130410     C***                GOTO      ENDCCV
103400130410     c***                endif
103500130410     c****               endif
103600130410     C****               ENDIF
103700060302     c* per bolle partenze (prepagati)
103800060303     c                   if        §7lvfa = 'F' and §7lprp = 'S'
103900060302     c                   z-add     blpaas        prmaas
104000060302     c                   z-add     blplnp        prmlnp
104100060302     c                   z-add     blpnrs        prmnrs
104200060302     c                   z-add     blpnsp        prmnsp
104300060302     c                   z-add     ar6fiv        prmfiv
104400060302     c                   z-add     ar6nft        prmnft
104500060302     c                   z-add     wdft          prmdft
104600060302     c                   clear                   prmesito
104700060608     c                   call      'YCOW0121R1'
104800060302     C                   PARM                    prmaas
104900060302     C                   PARM                    prmLnp
105000060302     C                   PARM                    prmNrs
105100060302     C                   PARM                    prmNsp
105200060302     C                   PARM                    prmFiv
105300060302     C                   PARM                    prmDft
105400060302     C                   PARM                    prmNft
105500060302     C                   PARM                    prmEsito
105600060302     c                   if        prmesito <> 0
105700060302     C                   MOVEL     msg(17)       d66msg
105800060302     C                   SETON                                        90
105900060302     C                   GOTO      endccv
106000060302     c                   endif
106100060302     c                   endif
106200950331     C* SE BOLLA IN ASSEGNATO MA NON ANCORA ABILITATO, NON POOSO UTILIZ
106300941013     C*  UNA CAUSALE DI TASSAZIONE RITASSAZIONE BOLLA
106400060303     C     ARBACA        ifeq      ' '
106500941013     C     §7LRTA        IFEQ      'S'
106600941013     C     §7LBFI        OREQ      'S'
106700941013     C     §7LBSF        OREQ      'S'
106800941013     C                   SETON                                        90
106900941013     C                   MOVEL     MSG(10)       D66MSG
107000941013     C                   GOTO      ENDCCV
107100941013     C                   ENDIF
107200941013     C                   ENDIF
107300021128      **
107400021128      * C O N S E G N A   P A R T I C O L A R E
107500021128      **
107600021128      * Se presente la  particolare "Supermercati" la causale deve
107700021128      * essere abilitata
107800021128     c                   If         §7lSup = 'N' and
107900021128     c                             (Wtc1 = 'S' or Wtc2 = 'S')
108000021128     c                   Eval      *In90 = *On
108100021128     c                   Eval      D66Msg = Msg(20)
108200021128     c                   GoTo      EndCcv
108300021128     c                   endif
108400941013     C*
108500941013     C                   ENDIF
108600941013     C*
108700941013     C     ENDCCV        ENDSR
108800061026     c*
108900061026     c* controllo DS7Lcampo  ---------------------------------------------*
109000061026     c     CTRds7lcampo  BEGSR
109100061026     c                   clear                   tipo
109200061026     c                   clear                   flag
109300061026     c* 1° flag --> §7LVDOPO variabile prima o dopo
109400061026     c*             Non variabile DOPO
109500061026    2c                   if        (§7lvdopo =' ' or §7lvdopo='N') and
109600061026     c                             wdata>0
109700061026     c                   movel     '1'           flag
109800061026     c                   movel     'P'           tipo
109900061026     c                   seton                                        90
110000061026     C                   GOTO      ENDctr
110100061026    2C                   ENDIF
110200061026     c
110300061026     c*             Variabile SOLO DOPO
110400061026    2c                   if        §7lvdopo ='C' and wdata=0
110500061026     c                   movel     '1'           flag
110600061026     c                   movel     'D'           tipo
110700061026     C                   SETON                                        90
110800061026     C                   GOTO      ENDctr
110900061026    2C                   ENDIF
111000061108     c
111100061108     c                   if        wdata>0
111200061026     c*
111300061026     c* 2° flag --> se pieno Include o ESclude alcuni tipi bolla
111400061026     c                   setoff                                       90
111500061108     c
111600061026     c* 3° flag --> tipo bolla da includere/escludere
111700061026    2c                   if        §7lines='I'
111800061026    3c                   if        §7lfras='F'  and flgtb1<>'F'
111900061026     C                   SETON                                        90
112000061026    3c                   endif
112100061026    3c                   if        §7lfras='A'  and flgtb1<>'A'
112200061026     C                   SETON                                        90
112300061026    3c                   endif
112400061026    3c                   if        §7lfras='E'  and §3atb2='  '
112500061026     C                   SETON                                        90
112600061026    3c                   endif
112700061026    3c                   if        §7lfras='S'  and §3atb2='  '
112800061026     c                             and flgtb1='F'
112900061026     C                   SETON                                        90
113000061026    3c                   endif
113100061026     c
113200061026     c* 4° flag -->  includi codificati o non
113300061026    3c                   if        not *in90 and §7lcodif='C'
113400061026     c                   EXSR      Codificato
113500061026    3c                   endif
113600061026     c
113700061026    3c                   if        not *in90 and §7lcodif='N'
113800061026     c                   exsr      NonCodificato
113900061026    3c                   endif
114000061026     c
114100061026    2c                   endif
114200061026    c
114300061026    2c                   if        §7lines='E'
114400061026    3c                   if        §7lfras='F'  and flgtb1='F'
114500061026     C                   SETON                                        90
114600061026    3c                   endif
114700061026    3c                   if        §7lfras='A'  and flgtb1='A'
114800061026     C                   SETON                                        90
114900061026    3c                   endif
115000061026    3c                   if        §7lfras='E'  and §3atb2<>'  '
115100061026     C                   SETON                                        90
115200061026    3c                   endif
115300061026    3c                   if        §7lfras='S'  and (§3atb2<>'  '
115400061026     c                             or  flgtb1='A')
115500061026     C                   SETON                                        90
115600061026    3c                   endif
115700061026    c
115800061026     c* 4° flag --> esclcudi codificati o non
115900061026    3c                   if        not *in90 and §7lcodif='C'
116000061026     c* Devo escludere i non codificati
116100061026     c                   exsr      NonCodificato
116200061026    3c                   endif
116300061026     c
116400061026    3c                   if        not *in90 and §7lcodif='N'
116500061026     c                   exsr      COdificato
116600061026    3c                   endif
116700061026     c
116800061026    2c                   endif
116900061026     C*
117000061026    2C     *IN90         IFEQ      *ON
117100061026     c                   movel     '2'           flag
117200061026     c                   movel     'D'           tipo
117300061026     C                   GOTO      ENDctr
117400061026    2C                   ENDIF
117500061108    2C                   ENDIF
117600061026     c     ENDCTR        ENDSR
117700061026     c
117800061026     c* OK se bolla     codificata ----------------------------------*
117900061026     c     Codificato    BEGSR
118000061026     c*  di 1°bolla
118100061026    4c                   if        §7lfras<>'E' and (wfineksc1=8888 or
118200061026     c                                              wfineksc1=9999)
118300061026     c                   seton                                        90
118400061026    4c                   endif
118500061026     c*  di 2°bolla
118600061026    4c                   if        (§7lfras='E' or §7lfras='S' or
118700061026    4c                              §7lfras=' ') and
118800061026     c                             wfineksc2=9999
118900061026     c                   seton                                        90
119000061026    4c                   endif
119100061026     c                   ENDSR
119200061026     c
119300061026     c* OK se bolla non codificata -----------------------------------*
119400061026     c     nonCodificato begsr
119500061026     c*  di 1°bolla
119600061026    4c                   if        §7lfras<>'E' and (wfineksc1<>8888 and
119700061026     c                                              wfineksc1<>9999)
119800061026     c                   seton                                        90
119900061026    4c                   endif
120000061026     c*  di 2°bolla
120100061026    4c                   if        (§7lfras='E' or §7lfras='S' or
120200061026    4c                              §7lfras=' ') and
120300061026     c                             wfineksc2<>9999 and wfineksc2>0
120400061026     c                   seton                                        90
120500061026    4c                   endif
120600061026     c                   ENDSR
120700061026     c*
120800061026     c* Decodifica la DS7Lcampo ------------------------------------------*
120900061026     c     Decodi7LCAMPO BEGSR
121000061026     c                   clear                   des28
121100061031     c                   if        §7lines<>' '
121200061031     c
121300061026     c                   if        §7lines='I'
121400061026     c                   movel     'Includi'     des28
121500061026     c                   endif
121600061026     c                   if        §7lines='E'
121700061026     c                   movel     'Escludi'     des28
121800061026     c                   endif
121900061026     c
122000061026     c                   select
122100061026     c                   when      §7lfras='F'
122200061031     c                   eval      des28=%trim(des28)+' Franchi '
122300061026     c                   when      §7lfras='A'
122400061031     c                   eval      des28=%trim(des28)+' Asseg.1Bol'
122500061026     c                   when      §7lfras='E'
122600061031     c                   eval      des28=%trim(des28)+' Asseg.2Bol'
122700061026     c                   when      §7lfras='S'
122800061031     c                   eval      des28=%trim(des28)+' Asseg1/2Bo'
122900061026     c                   when      §7lfras=' '
123000061031     c                   eval      des28=%trim(des28)+' TutteBolle '
123100061026     c                   endsl
123200061026     c
123300061026     c                   if        §7lcodif='C'
123400061031     c                   eval      des28=%trim(des28)+' Codific'
123500061026     c                   endif
123600061026     c                   if        §7lcodif='N'
123700061031     c                   eval      des28=%trim(des28)+' NoCodif'
123800061026     c                   endif
123900061031     c                   endif
124000061026     c                   ENDSR
124100941013     C*
124200060302     C* CONtROLLO SE EFFETTUATA SELEZIONE ----------------------------*
124300920323     C     CONTR         BEGSR
124400941013     C                   READC     LR66DSF                                31
124500920323     C*
124600941013     C     *IN31         DOWEQ     *OFF
124700941013     C*
124800920323     C     VIDSEL        IFEQ      '1'
124900941013     C                   MOVEL     VIDCOD        D66CVB
125000941013     C                   MOVEL     DESCOD        D66DES
125100941013     C                   MOVEL     FLGFFI        D66FFI
125200941013     C                   MOVEL     FLGC4V        D66C4V
125300941013     C                   MOVEL     FLGSTF        D66STF
125400060130     C                   MOVEL     FLGPOCL       D66POCL
125500941013     C                   MOVEL     FLGFSA        D66FSA
125600941013     C                   MOVEL     FLGRTA        D66RTA
125700060130     C                   MOVEL     FLGMTV        D66MTV
125800920323     C                   SETON                                        31
125900941013     C                   ENDIF
126000920323     C*
126100941013     C  N31              READC     LR66DSF                                31
126200941013     C                   ENDDO
126300920323     C                   ENDSR
126400130405     C* Apertura titas -----------------------------------------------*
126500130405     C     opnfile       BEGSR
126600130405     c                   if        not %open(titas30c)
126700130405     c                   if        %subst(knsif:7:1)='P'
126800130405     c                   eval      wlibsede=wlibsedep
126900130405     c                   else
127000130405     c                   eval      wlibsede=wlibsedeb
127100130405     c                   endif
127200130405     c                   open      titas30c
127300130405     c                   endif
127400130405     c                   endsr
127500130409     C* Chain titas  per controllo se contabilizzata -----------------*
127600130409     C     chtitas       BEGSR
127700130418     c                   eval      ktbl1=%subst(ktbl:1:1)
127800130418     c     ktas          setll     titas30c
127900130418     c     ktas1         reade     titas30c
128000130419    1c                   if        not %eof(titas30c) and
128100130419     c                             %subst(tastbl:1:1)=ktbl1
128200130418
128300130410     c* BOLLA CONTABILIZZATA
128400130410     c* --> sempre errore se non variabile dopo contabilizzazione
128500130410    2c                   if        tasfic<>*blanks and §7lvdf=*blanks
128600130409     c                   Movel     Msg(11)       D66Msg
128700130409     c                   Eval      *In90 = *On
128800130410   x2c                   else
128900130410     c* BOLLA NON CONTABILIZZATA oppure VARIABILE DOPO CONTABILIZZ.
129000130410     c* Se variazione di tassazione
129100130415    3c                   if        %subst(vidcod:1:1) = 'T'
129200130410     c* Bolla     Contabilizzata: si può variare solo ias e vmd
129300130410    4c                   if        tasfic<>*blanks
129400130410     c                   eval      d66bdi='S'
129500130410   x4c                   else
129600130410     c* Bolla non Contabilizzata: controllo incasso e se già incassata
129700130410     c* si può variare solo IAS e VMD
129800130410    5c                   if        §7lbfi='S'
129900130410    6c                   if        arbica<>' ' and arbica <>'R'
130000130410    7c                   if        §7lvdf='S'
130100130409     c                   eval      d66bdi='S'
130200130410   x7c                   else
130300130410     c* errore se non variabile dopo incasso
130400130410     c                   Movel     Msg(17)       D66Msg
130500130410     c                   Eval      *In90 = *On
130600130410    7c                   endif
130700130410    6c                   endif
130800130410    6c                   if        arbica='R'
130900130409     C     KARI          CHAIN     FIARI01L
131000130410    7c                   if        %found(fiari01l) and ariatb=' '
131100130410    8c                   if        §7lvdf='S'
131200130409     c                   eval      d66bdi='S'
131300130410     c* errore se non variabile dopo incasso
131400130410     c                   Movel     Msg(17)       D66Msg
131500130410     c                   Eval      *In90 = *On
131600130410   x8c                   else
131700130410     c                   endif
131800130410    7c                   endif
131900130410    6c                   endif
132000130410    5c                   endif
132100130410    4c                   endif
132200130409    3c                   endif
132300130409    2c                   endif
132400130409    1c                   endif
132500130409     c                   endsr
132600000510**
132700941013Bolla inesistente                                                              1
132800941013Causale Variazione inesistente                                                 2
132900151218Causale Variazione non utilizzabile                                            3
133000941013Causale non utilizzabile: spedizione giacente.Variare eseguendo le disposizion 4
133100950131Causale Variazione per C/Assegno ma la bolla e' senza C/A o e' gia' incassato  5
133200941013Il codice bolla non puo' essere modificato per una spedizione locale           6
133300941013Causale non utilizzabile: Spedizione gia' consegnata                           7
133400941013Causale utilizzabile soltanto dopo la consegna completa o parziale, della bolla8
133500941013Causale non utilizzabile: e' gia' stata eseguita una consegna parziale         9
133600941013Per eseguire questo tipo di variazione tassare prima la bolla                 10
133700941013Causale non utilizzabile: spedizione gia' contabilizzata                      11
133800941013Causale utilizzabile soltanto dopo la contabilizzazione della bolla           12
133900941013Causale non utilizzabile per il tipo di bolla richiesto                       13
134000941013Causale utilizzabile soltanto per bolle Segue Fattura                         14
134100941013Causale utilizzabile soltanto per bolle con fattura immediata                 15
134200950120BOLLA LEGATA:il C/Assegno e' da variare sulla bolla MAMMA xx xxx xx xxxxxxx   16
134300950331Causale non utilizzabile: Spedizione gia' incassata                           17
134400981221Per i Cambi di Porto impossibile rimodificare il tipo bolla della bolla mamma
134500151218Causale Variazione utilizzabile solo per bolle presenti in arrivo             19
134600021128Causale non utilizzabile: spedizione con consegna particolare "SUPERMERCATI"  20
134700030127Causale non utilizzabile: spedizione senza bancali                            21
134800030127Causale non utilizzabile: variazione già effettuata                           22
134900061025Causale non utilizzabile dopo CONSEGNA per il tipo di bolla richiesto         23
135000061026Causale non utilizzabile dopo CONSEGNA PARZIALE per il tipo di bolla richiesto
135100070305Causale non utilizzabile: spedizione stornata (non viene fatturata)           25
135200151218Bolla non manutenzionabile per questa Causale Variazione                      26
135300151218Bolla non manutenzionabile: chiusa in partenza per "Merce mai Affidata"       27
