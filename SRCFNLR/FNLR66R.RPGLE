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
008900160915     C* 36    - off emissine sfldsp
009000941013     C* 90    - ERORRE GENERICO
009100941012     C*---------------------------------------------------------------*
009200000000     C     *ENTRY        PLIST
009300000000     C                   PARM                    KPJBA
009400941012     C                   MOVEL     KPJBU         DSLR66
009500941012     C*
009600000000     C                   Z-ADD     1             CODUT
009700000000     C                   CALL      'XPARUT'
009800000000     C                   PARM                    UTEDSE
009900941012     C                   MOVEL     RAGUT         VIDDSA
010000000104     C                   MOVEL     DTU(10)       W0020             2 0
010100000104     C                   Z-ADD     DTU(10)       WDTU10
010200000104     C     W0020         IFGE      60
010300000104     C                   MOVEL     19            WDTU10
010400000104     C                   ELSE
010500000104     C                   MOVEL     20            WDTU10            8 0
010600000104     C                   ENDIF
010700941012     C*---------------------------------------------------------------*
010800941012     C* TIPO LANCIO, D15TLA "C" -           CHIUSO CON LR
010900941012     C* TIPO LANCIO, D15TLA "L" - ELABORO E CHIUDO CON LR
011000941012     C* TIPO LANCIO, D15TLA " " - ELABORO E CHIUDO IN RETRN
011100941012    1C     D66TLA        IFNE      'C'
011200160915     C                   SETOFF                                       9036
011300001213    2C     D66GES        IFNE      'C'
011400140506    2C     D66GES        andne     'K'
011500001213    3C     OPN66         IFEQ      '0'
011600001213     C                   OPEN      FNLR66D
011700001213     C                   MOVE      '1'           OPN66
011800001213    3C                   ENDIF
011900001213   X2C                   ELSE
012000001213    3C     OPN66         IFEQ      '1'
012100001213     C                   CLOSE     FNLR66D
012200001213     C                   MOVE      '0'           OPN66
012300001213    3C                   ENDIF
012400001213    2C                   ENDIF
012500941012     C*
012600941012     C* VISUALIZZAZIONE PER SCELTA CODICE VARIAZIONE:
012700941013    2C     D66GES        IFEQ      'V'
012800941012     C* CARICO LE CAUSALI
012900920323     C                   EXSR      CARCOD
013000910219     C*
013100941013     C                   WRITE     LR66D02
013200160915     c                   if        nrr=0
013300160915     c                   seton                                        36
013400160915     c                   endif
013500160915
013600900509     C     FOR01         TAG
013700941013     C                   EXFMT     LR66DCT
013800910912     C** CMD12 - RITORNO
013900920323     C  NKL              DO
014000910806     C* ENTER
014100920323     C                   EXSR      CONTR
014200941013     C     D66CVB        IFEQ      '  '
014300910219     C                   GOTO      FOR01
014400941013     C                   ENDIF
014500941013     C*
014600941013     C                   ENDDO
014700941013     C* SE IN VISUALIZZAZIONE TROVATO QUALCHE ERRORE NON MI INTERESSA
014800941013     C                   CLEAR                   D66MSG
014900941013     C*
015000941013  X2 C                   ELSE
015100941013     C*
015200941013     C* CONTROLLO LA CAUSALE VARIAZIONE ANCHE CON BOLLA SE IMMESSA
015300941013     C                   MOVEL     '7L'          COD
015400941013     C                   MOVEL     D66CVB        KEY
015500941013     C     KTAB2         CHAIN     TABEL                              30
015600941013     C*
015700941013    3C     *IN30         IFEQ      *ON
015800941013     C     *IN30         OREQ      *OFF
015900941013     C     TBLFLG        ANDNE     *BLANKS
016000941013     C                   MOVEL     MSG(2)        D66MSG
016100941013     C                   SETON                                        90
016200941013    3C                   ENDIF
016300941013     C*
016400941013    3C     *IN90         IFEQ      *OFF
016500941013     C                   MOVEL     TBLUNI        DS7L
016600941013     C                   MOVEL     §7LDES        D66DES
016700941013     C                   MOVEL     §7LFFI        D66FFI
016800941013     C                   MOVEL     §7LC4V        D66C4V
016900941013     C                   MOVEL     §7LSTF        D66STF
017000060130     C                   MOVEL     §7LPOCL       D66POCL
017100941013     C                   MOVEL     §7LFSA        D66FSA
017200941013     C                   MOVEL     §7LRTA        D66RTA
017300941102     C                   MOVEL     §7LBSF        D66BSF
017400130409     C                   MOVEL     §7LBFI        D66BFI
017500130409     C****               MOVEL     §7LFIN        D66BDI
017600060130     C                   MOVEL     §7LMTV        D66MTV
017700130411     C                   MOVEL     §7Lpda        D66pda
017800151218     c                   clear                   d66utp
017900941013     C* CONTROLLO BOLLA
018000941013     C                   EXSR      CTRBOL
018100941013     c*
018200941013    4C     *IN90         IFEQ      *OFF
018300941013     C                   MOVEL     D66CVB        VIDCOD
018400941013     C                   EXSR      CTRCVB
018500941013    4C                   ENDIF
018600941013    3C                   ENDIF
018700941013     C*
018800941013    2C                   ENDIF
018900941013    1C                   ENDIF
019000910219     C*
019100941013     C     D66MSG        IFNE      *BLANKS
019200941013     C                   MOVEL     '1'           D66ERR
019300941013     C                   ENDIF
019400941013     C*
019500941013     C                   MOVEL     DSLR66        KPJBU
019600941013     C* CHIUSURA PGM
019700941013    1C     D66TLA        IFEQ      ' '
019800941013     C                   RETURN
019900941013     C                   ELSE
020000001213     C*
020100001213     C     OPN66         IFEQ      '1'
020200001213     C                   CLOSE     FNLR66D
020300001213     C                   ENDIF
020400130405
020500130405     c                   if        %open(titas30c)
020600130405     c                   close     titas30c
020700130405     c                   endif
020800001213     C*
020900941013     C                   SETON                                        LR
021000941013    1C                   ENDIF
021100941013     C*
021200941013     C* OPERAZIONI INIZIALI ------------------------------------------*
021300941013     C     *INZSR        BEGSR
021400941013     C     KTAB          KLIST
021500941013     C                   KFLD                    CODUT
021600941013     C                   KFLD                    COD
021700941013     C     KTAB2         KLIST
021800941013     C                   KFLD                    CODUT
021900941013     C                   KFLD                    COD
022000941013     C                   KFLD                    KEY
022100941013     C     KBOL          KLIST
022200941013     C                   KFLD                    D66AAS
022300941013     C                   KFLD                    D66LNP
022400941013     C                   KFLD                    D66NRS
022500941013     C                   KFLD                    D66NSP
022600991011     C     KBOL6         KLIST
022700991011     C                   KFLD                    D66AAS
022800991011     C                   KFLD                    D66LNP
022900991011     C                   KFLD                    D66NRS
023000991011     C                   KFLD                    D66NSP
023100991011     C                   KFLD                    WTRC
023200030124
023300030124     c     kAr5          Klist
023400030124     c                   Kfld                    D66Aas
023500030124     c                   Kfld                    D66Lnp
023600030124     c                   Kfld                    D66Nrs
023700030124     c                   Kfld                    D66Nsp
023800030124     c                   Kfld                    kAr5Trd
023900130405     C     Ktas          KLIST
024000130405     C                   KFLD                    D66AAS
024100130405     C                   KFLD                    D66LNP
024200130405     C                   KFLD                    D66NRS
024300130405     C                   KFLD                    D66NSP
024400130418     C                   KFLD                    ktbl1
024500130419     C     Ktas1         KLIST
024600130419     C                   KFLD                    D66AAS
024700130419     C                   KFLD                    D66LNP
024800130419     C                   KFLD                    D66NRS
024900130419     C                   KFLD                    D66NSP
025000130409     C     Kari          KLIST
025100130409     C                   KFLD                    D66AAS
025200130409     C                   KFLD                    D66LNP
025300130409     C                   KFLD                    D66NRS
025400130409     C                   KFLD                    D66NSP
025500130409     C                   KFLD                    wtip
025600941013     C* *LIKE DEFN
025700941013     C     *LIKE         DEFINE    TBLKEY        KEY
025800941013     C     *LIKE         DEFINE    TBLCOD        COD
025900941013     C     *LIKE         DEFINE    ARBLNA        WLNA
026000941013     C     *LIKE         DEFINE    ARBLNP        WLNP
026100941013     C     *LIKE         DEFINE    ARBDCM        WDCM
026200941013     C     *LIKE         DEFINE    ARBICC        WICC
026300941013     C     *LIKE         DEFINE    ARBDCP        WDCP
026400981221     C     *LIKE         DEFINE    ARBCCA        WCCA
026500991011     C     *LIKE         DEFINE    AR6TRC        WTRC
026600001213     C*
026700001213     C                   MOVE      '0'           OPN66             1
026800941013     C*
026900941013     C                   ENDSR
027000910205     C*
027100941012     C* CARICO LE CAUSALI VARIAIZONI ---------------------------------*
027200920323     C     CARCOD        BEGSR
027300941209     C                   CLEAR                   VIDSEL
027400941012     C*
027500941012     C                   SETON                                        35
027600941013     C                   WRITE     LR66DCT
027700160915     C                   SETOFF                                       3536
027800941012     C*
027900910205     C                   Z-ADD     0             NRR               4 0
028000941013     C*
028100941013     C* CARICO I DATI BOLLA SE C'E'
028200941013     C                   EXSR      CTRBOL
028300941012     C*
028400910806     C                   SETOFF                                       30
028500920323     C                   MOVEL     '7L'          COD
028600920323     C     KTAB          SETLL     TABEL
028700920323     C     KTAB          READE     TABEL                                  30
028800910220     C*
028900941012    1C     *IN30         DOWEQ     *OFF
029000061031     c                   clear                   descon
029100061031     c                   clear                   desconp
029200920323     C*
029300941012     C* ESCLUDO GLI ANNULLATI
029400941012    2C     TBLFLG        IFEQ      ' '
029500920323     C                   MOVEL     TBLUNI        DS7L
029600941012     C                   MOVEL     TBLKEY        VIDCOD
029700941013     C                   MOVEL     §7LDES        DESCOD
029800941013     C                   SETOFF                                       90
029900941013     C* CONTROLLO CON BOLLA
030000950706     C                   MOVEL     VIDCOD        FLGCVB
030100941013     C                   EXSR      CTRCVB
030200941013     C*
030300941013    3C     *IN90         IFEQ      *OFF
030400941012     C*
030500941012     C* UTILIZZAZIONE IN PARTENZA
030600941013    4C     §7LUCP        IFNE      'N'
030700941012     C                   MOVEL     'SI'          VIDUCP
030800941012     C                   ELSE
030900941012     C                   MOVEL     '  '          VIDUCP
031000941012    4C                   ENDIF
031100941013     C*
031200941012     C* UTILIZZAZIONE IN ARRIVO
031300941013    4C     §7LUCV        IFNE      'N'
031400941012     C                   MOVEL     'SI'          VIDUCV
031500941012     C                   ELSE
031600941012     C                   MOVEL     '  '          VIDUCV
031700941012    4C                   ENDIF
031800060130     C* CONTROLLA ANCHE P.O. CLIENTE
031900060130    4C     §7LPOCL       IFEQ      'S'
032000060130     C                   MOVEL     'SI'          VIDPOCL
032100060130     C                   ELSE
032200060130     C                   MOVEL     '  '          VIDpocl
032300060130    4C                   ENDIF
032400060130     C
032500941013     C*
032600920323     C* VARIAZIONE DOPO CONSEGNA
032700061026     c                   movel     §7lvdco       ds7lcampo
032800061026    4C     §7Lvdopo      IFEQ      'S'
032900061026     C                   MOVEL     '  SI '       VIDVDC
033000061026     C                   ELSE
033100061026    5C     §7Lvdopo      IFEQ      'C'
033200061026     C                   MOVEL     ' SOLO'       VIDVDC
033300061026     C                   ELSE
033400061026     C                   MOVEL     '  NO '       VIDVDC
033500061026    5C                   END
033600061026    4C                   END
033700061026    4C**   §7LVDC        IFEQ      'S'
033800061026     C**                 MOVEL     ' SI '        VIDVDC
033900061026     C**                 ELSE
034000061026    5C**   §7LVDC        IFEQ      'C'
034100061026     C*+                 MOVEL     'SOLO'        VIDVDC
034200061026     C**                 ELSE
034300061026     C**                 MOVEL     ' NO '        VIDVDC
034400061026    5C**                 END
034500061026    4C**                 END
034600061026     c                   exsr      Decodi7LCAMPO
034700061026     c                   if        des28<>*blanks
034800061031     c
034900061031     c                   move      vidvdc        alfa4             4
035000061031     c                   if        %subst(vidvdc:2:1)=' '
035100061031     c                   movel     ' *'          vidvdc
035200061031     c                   else
035300061031     c                   movel     '*'           vidvdc
035400061031     c                   endif
035500061031     c
035600061031     c                   eval      descon='DopCON:'+alfa4
035700061031     c                   eval      descon=%trim(descon)+' '+des28
035800061026     c                   endif
035900941012     C*
036000941012     C* VARIAZIONE DOPO CONSEGNA PARZIALE
036100061026     c                   movel     §7lvdcop      ds7lcampo
036200061026    4C     §7Lvdopo      IFEQ      'S'
036300061031     C                   MOVEL     ' SI  '       VIDVDP
036400941012     C                   ELSE
036500061026    5C     §7Lvdopo      IFEQ      'C'
036600061031     C                   MOVEL     ' SOLO'       VIDVDP
036700941012     C                   ELSE
036800061031     C                   MOVEL     ' NO  '       VIDVDP
036900941013    5C                   END
037000941013    4C                   END
037100061026    4C**   §7LVDP        IFEQ      'S'
037200061026     C**                 MOVEL     ' SI '        VIDVDP
037300061026     C**                 ELSE
037400061026    5C**   §7LVDP        IFEQ      'C'
037500061026     C**                 MOVEL     'SOLO'        VIDVDP
037600061026     C**                 ELSE
037700061026     C**                 MOVEL     ' NO '        VIDVDP
037800061026    5C**                 END
037900061026    4C**                 END
038000061026     c**
038100061026     c                   exsr      Decodi7LCAMPO
038200061026     c
038300061026     c                   if        des28<>*blanks
038400061026     c                   movel     '*'           vidvdp
038500061026     c                   move      vidvdp        alfa4
038600061031     c                   eval      desconp='DopPARZ:'+alfa4+des28
038700061026     c                   endif
038800941012     C*
038900920323     C* VARIAZIONE DOPO CONTABILIZZAZIONE
039000941013    4C     §7LVDF        IFEQ      'S'
039100941014     C                   MOVEL     ' SI '        VIDVDF
039200920323     C                   ELSE
039300941013    5C     §7LVDF        IFEQ      'C'
039400920323     C                   MOVEL     'SOLO'        VIDVDF
039500920323     C                   ELSE
039600941014     C                   MOVEL     ' NO '        VIDVDF
039700941013    5C                   END
039800941013    4C                   END
039900941012     C*
040000941013     C* FRANCHI/ ASSEGNATI/ 2 BOLLA
040100941013     C                   CLEAR                   DESTBL
040200941013    4C     §7LVFA        IFEQ      'F'
040300060301     c                   select
040400060301     c                   when      §7lprp = 'S'
040500060301     c                   eval      destbl='FRANCO Prepag'
040600060301     c                   when      §7lprp = 'N'
040700060301     c                   eval      destbl='FRANCO NoPrep'
040800060301     c                   when      §7lprp = ' '
040900060301     c                   eval      destbl='FRANCO       '
041000060301     c*****              eval      destbl='FRANCO'
041100060301     c                   endsl
041200920323     C                   ELSE
041300941013    5C     §7LVFA        IFEQ      'A'
041400941013     C                   MOVEL     'ASSEG'       DESTBL
041500920323     C                   ELSE
041600941013    6C     §7LVFA        IFEQ      'E'
041700941013     C                   MOVEL     '2 BOL'       DESTBL
041800920323     C                   ELSE
041900941013     C                   MOVEL     'TUTTE'       DESTBL
042000941013    6C                   END
042100941013    5C                   END
042200941013    4C                   END
042300941013     C*
042400941013     C* VARIAZIONE PER BOLLE CON C/ASS
042500941013     C     §7LFCA        IFEQ      'S'
042600060126     c                   eval      destbl=%trim(destbl)+(' con C/A')
042700941013     C                   ENDIF
042800941013     C*
042900920323     C* PER BOLLA SEGUE FATTURA
043000941013    4C     §7LBSF        IFEQ      'S'
043100060126     c                   eval      destbl=%trim(destbl)+(' S.F.')
043200941013    4C                   END
043300920323     C* PER BOLLA FATTURA IMMEDIATA
043400941013    4C     §7LBFI        IFEQ      'S'
043500060126     c                   eval      destbl=%trim(destbl)+(' FT.IMM')
043600941013    4C                   END
043700060130     C* DA STORICIZZARE ora flag unico
043800941013    4C     §7LFSA        IFEQ      'S'
043900941012     C                   MOVEL     'SI'          VIDFSA
044000941012     C                   ELSE
044100941012     C                   MOVEL     '  '          VIDFSA
044200941013    4C                   END
044300060130     c
044400941012     C* DA STORICIZZARE ALLA PARTENZA
044500060130    4C**   §7LFSP        IFEQ      'S'
044600060130     C**                 MOVEL     'SI'          VIDFSP
044700060130     C**                 ELSE
044800060130     C**                 MOVEL     '  '          VIDFSP
044900060130    4C**                 END
045000060130     C* MEMORIZZAZIONE MOTIVO VARIAZIONE
045100060130    4C     §7LMTV        IFEQ      'S'
045200060130     C                   MOVEL     'SI'          VIDMTV
045300060130     C                   ELSE
045400060130     C                   MOVEL     '  '          VIDMTV
045500060130    4C                   END
045600910912     C*
045700941013     C                   MOVEL     §7LFFI        FLGFFI
045800941013     C                   MOVEL     §7LC4V        FLGC4V
045900941013     C                   MOVEL     §7LSTF        FLGSTF
046000060130     C                   MOVEL     §7LPOCL       FLGPOCL
046100941013     C                   MOVEL     §7LFSA        FLGFSA
046200941013     C                   MOVEL     §7LRTA        FLGRTA
046300060130     C                   MOVEL     §7LMTV        FLGMTV
046400910205     C* SCRIVO SUBFILE
046500910205     C                   ADD       1             NRR
046600941013     C                   WRITE     LR66DSF
046700941013    3C                   END
046800941013    2C                   END
046900910912     C*
047000920323     C     KTAB          READE     TABEL                                  30
047100941013    1C                   ENDDO
047200910205     C*
047300910205     C                   ENDSR
047400941013     C*
047500941013     C* CARICO LA BOLLA ----------------------------------------------*
047600941013     C     CTRBOL        BEGSR
047700941013     C                   SETOFF                                       01
047800941217     C                   CLEAR                   WGIAC
047900061026     C                   CLEAR                   ds3a
048000061026     C                   CLEAR                   Wfineksc1
048100061026     C                   CLEAR                   Wfineksc2
048200151218     C                   CLEAR                   Wft1
048300151218     C                   CLEAR                   Wbopar
048400941013     C*
048500941013     C* SE IMMESSA LA BOLLA FACCIO CONTROLLO INCROCIATI
048600941013    1C     D66NSP        IFGT      0
048700941013     C* BOLLA ARRIVI
048800941013    2C     D66TBO        IFEQ      'A'
048900941013     C     KBOL          CHAIN     FNARB000                           30
049000941013     C*
049100941013    3C     *IN30         IFEQ      *OFF
049200941013     C                   SETON                                        01
049300060301     c                   movel     arbcbo        wcbo
049400991011     C                   MOVEL     '1'           WTRC
049500991011     C     KBOL6         CHAIN     FIAR6000                           30
049600941013     C  N30              MOVE      AR6DFT        WDFT                           D.FATT 1 BOL
049700941013     C   30              CLEAR                   WDFT
049800061026     c* codice segue fattura/fattura immediata
049900061026     c  n30              move      ar6ksc        wfineksc1         4 0
050000061026     c   30              move      arbksc        wfineksc1
050100941013     C                   MOVEL     ARBDCM        WDCM                           D.CONSEGNA
050200941013     C                   MOVEL     ARBDCP        WDCP                           D.CONS.PARZ
050300941013     C                   MOVEL     ARBLNP        WLNP                           LIN PARTENZA
050400941013     C                   MOVEL     ARBLNA        WLNA                           LIN ARRIVO
050500941013     C                   MOVEL     ARBICC        WICC                           INCASSO C/A
050600981221     C                   MOVEL     ARBCCA        WCCA                           CONS.ANOMALA
050700021128      * Imposto flag consegne particolari
050800021128     c                   Eval      Wtc1 = ArbTc1
050900021128     c                   Eval      Wtc2 = ArbTc2
051000021128
051100941013     C*
051200941013     C* DATA FATTURA: SE 2 BOLLA LA PRENDO DALLA ESTESIONE
051300941013     C                   MOVEL     '3A'          COD                            COD BOLLA
051400941013     C                   MOVEL(P)  ARBCBO        KEY
051500941013     C     KTAB2         CHAIN     TABEL                              31
051600941013     C  N31              MOVEL     TBLUNI        DS3A
051700941013     C*
051800941013     C* VEDO SE GIACENTE
051900050331     C     KBOL          CHAIN     Tigcp21L                           31        GIACENZA
052000941013    4C     *IN31         IFEQ      *OFF
052100050331     C     GCPFAS        ANDLT     40
052200941013     C     GCPATB        ANDEQ     ' '
052300941013     C                   MOVEL     'S'           WGIAC             1
052400941013    4C                   ENDIF
052500941013     C*
052600941013     C*GIRO DATA SPEDIZIONE
052700941013     C                   MOVEL     '3'           G02ERR
052800941013     C                   Z-ADD     ARBMGS        G02INV
052900941013     C                   MOVEL     ARBAAS        G02INV
053000941013     C                   CALL      'XSRDA8'
053100941013     C                   PARM                    WLBDAT
053200941013     C*
053300941013     C                   Z-ADD     G02DAT        VIDDSP
053400941013    3C                   ENDIF
053500151218     c*Se non trovata la bolla in arrivo e il chiamante ammette la manutenzione di
053600151218     c* bolle non ancora partite
053700160115     c* Per CVB che utilizza anche il p.o. cliente non permetto di andare su
053800160115     c* BLP perchè le variazioni di tassazione assegnati non le permettiamo su
053900160115     c* bolle ancora da partire
054000160115     c                   if        not *in01 and d66bnp='S' and d66pocl=*blanks
054100151218     c                   exsr      sr_dablp
054200151218     c                   if        *in01
054300151218     c                   eval      wbopar='S'
054400151218     c                   eval      wft1=blpft1
054500151218     c                   endif
054600151218     c                   endif
054700941013     C*
054800941013   X2C                   ELSE
054900151218     C* BOLLA PARTENZA
055000151218     c                   exsr      sr_dablp
055100941013     C*
055200941013    2C                   ENDIF
055300061026     c
055400061026     c* Chain su seconda bolla, se presente
055500061026    4C     §3ATB2        IFNE      *BLANKS
055600061026     C                   MOVEL     '2'           WTRC
055700061026     C     KBOL6         CHAIN     FIAR6000                           32
055800061026     C  N32              MOVE      AR6DFT        W2DFT             8 0          D.FATT 2 BOL
055900061026     C   32              CLEAR                   W2DFT
056000061026     c  n32              move      ar6ksc        wfineksc2         4 0
056100061026    4C                   ENDIF
056200941013     C*
056300941013     C* NON TROVATA BOLLA
056400941013    2C     *IN01         IFEQ      *OFF
056500060301     c                   clear                   wcbo
056600941013     C                   MOVEL     MSG(1)        D66MSG
056700941013     C                   SETON                                        90
056800941013   X2C                   ELSE
056900941013     C*
057000941013     C* IMPOSTO I FLAG CHE MI SERVONO PER I CONTROLLI
057100941013     C                   MOVEL     VIDCOD        FLGCVB            1
057200941013     C                   MOVEL     §3ATB1        FLGTB1            1
057300941013    2C                   ENDIF
057400030124
057500060301     c* se bolla avente codice "M"=C/S per prepagati annullati
057600060301     c* non è possibile effettuare alcuna variazione
057700060301     c                   if        wcbo = 'M '
057800060301     C                   MOVEL     MSG(13)       D66MSG
057900060301     C                   SETON                                        90
058000060301     c                   endif
058100030124      * Se richiesta causale x Bancali la bolla deve avere il record 'BAN'
058200030124      * in Fira5 e non deve essere già stato mod.
058300030124     c                   If        D66Cvb = 'BR'
058400030124     c                   Clear                   dAr5Ban
058500030124     c     Kar5          Chain     Fiar501l
058600030124     c                   If        Not %Found(Fiar501l)
058700030127     c                   Movel     Msg(21)       D66Msg
058800030124     c                   Eval      *In90 = *On
058900030124     c                   Else
059000030124     c                   Movel     Ar5Uni        dAr5Ban
059100030124     c                   If        §Ar5Rb1 > *Zeros or §Ar5Rb2 > *Zeros
059200030127     c                   Movel     Msg(22)       D66Msg
059300030124     c                   Eval      *In90 = *On
059400030124     c                   EndIf
059500030124     c                   EndIf
059600030124     c                   EndIf
059700941013     C*
059800941013    1C                   ENDIF
059900920323     C*
060000941013     C                   ENDSR
060100151218     C* Bolla da blp -------------------------------------------------*
060200151218     C     sr_dablp      BEGSR
060300151218     C* BOLLA PARTENZA
060400151218     C     KBOL          CHAIN     FNBLP000                           30
060500151218     C*
060600151218    3C     *IN30         IFEQ      *OFF
060700151218     C                   SETON                                        01
060800151218     c                   movel     blpcbo        wcbo
060900151218     C                   MOVEL     '1'           WTRC
061000151218     C     KBOL6         CHAIN     FIAR6000                           30
061100151218     C  N30              MOVE      AR6DFT        WDFT              8 0          D.FATT 1 BOL
061200151218     C   30              CLEAR                   WDFT
061300151218     c* codice segue fattura/fattura immediata
061400151218     c  n30              move      ar6ksc        wfineksc1         4 0
061500151218     c   30              move      arbksc        wfineksc1
061600151218     C                   MOVEL     BLPDCM        WDCM                           D.CONSEGNA
061700151218     C                   MOVEL     BLPDCP        WDCP                           D.CONS.PARZ
061800151218     C                   MOVEL     BLPLNP        WLNP                           LIN PARTENZA
061900151218     C                   MOVEL     BLPLNA        WLNA                           LIN ARRIVO
062000151218     C                   MOVEL     BLPCCA        WCCA                           CONS.ANOMALA
062100151218      * Imposto flag consegne particolari
062200151218     c                   Eval      Wtc1 = BlpTc1
062300151218     c                   Eval      Wtc2 = BlpTc2
062400151218
062500151218     C                   CLEAR                   W2DFT                          D.FATT 2 BOL
062600151218     C* COME SE FOSSE SEMPRE INCASSATO: NON SI PUO' VARIARE
062700151218     C                   MOVEL     'S'           WICC                           INCASSO C/A
062800151218     C* DATA FATTURA: SE 2 BOLLA LA PRENDO DALLA ESTESIONE
062900151218     C                   MOVEL     '3A'          COD
063000151218     C                   MOVEL(P)  BLPCBO        KEY
063100151218     C     KTAB2         CHAIN     TABEL                              31
063200151218     C  N31              MOVEL     TBLUNI        DS3A
063300151218     C*
063400151218     C* VEDO SE GIACENTE
063500151218     C     KBOL          CHAIN     Tigcp01L                           31
063600151218    4C     *IN31         IFEQ      *OFF
063700151218     C     GCPFAS        ANDLT     60
063800151218     C     GCPATB        ANDEQ     ' '
063900151218     C                   MOVEL     'S'           WGIAC             1
064000151218    4C                   ENDIF
064100151218     C*
064200151218     C*GIRO DATA SPEDIZIONE
064300151218     C                   MOVEL     '3'           G02ERR
064400151218     C                   Z-ADD     BLPMGS        G02INV
064500151218     C                   MOVEL     BLPAAS        G02INV
064600151218     C                   CALL      'XSRDA8'
064700151218     C                   PARM                    WLBDAT
064800151218     C*
064900151218     C                   Z-ADD     G02DAT        VIDDSP
065000151218    3C                   ENDIF
065100151218     c                   endsr
065200941013     C*
065300941013     C* CONTROLLO LA CAUSALE CON LA BOLLA IMMESSA --------------------*
065400941013     C     CTRCVB        BEGSR
065500941013     C*
065600941013     C                   SETOFF                                       90
065700000510     C*
065800000510     C     WLNP          CHAIN     AZORG01L                           30
065900000510     C  N30              MOVEL     ORGDE3        OG143
066000000510     C   30              CLEAR                   OG143
066100941013     C*
066200140502     c* i controlli seguenti li ignoro per le causali utilizzate solo
066300140502     c* per chiamate cieche (entrambi i codici UCP e UCV='N')
066400140506     c* per dare la possibilità al chiamante di richiamare LR66R per
066500140502     c* esecuzione dei controlli
066600140506     c                   if        d66ges<>'K' or §7lucv<>'N' or §7lucp<>'N'
066700941013     C* ESCLUDO LE BOLLE DA NON UTILIZZARE SE SO IL TIPO DI BOLLA
066800941013    1C     D66TBO        IFEQ      'A'
066900151217     c                   select
067000151218     c*- - - - - - - - - - - - - - - -
067100151218     c* cvb NON utilizzabile in arrivo
067200151218     c*       OPPURE
067300151218     c* cvb utilizzabile in arrivo ma solo su bolle in partenza ma pgm non abilitato
067400151218     c*- - - - - - - - - - - - - - - -
067500151218     C                   when      §7lucv='N' or
067600151218     C                             (d66bnp=*blanks and §7lucv='P')
067700941013     C                   SETON                                        90
067800941013     C                   MOVEL     MSG(3)        D66MSG
067900151218     c*- - - - - - - - - - - - - - - -
068000151218     c* cvb utilizzabile in arrivo ma SOLO su BLP NON TRASMESSO
068100151218     c* --> errore se bolla già in ARB oppure in BLP già trasmessa
068200151218     c*- - - - - - - - - - - - - - - -
068300151218     C                   when      *in01 and §7lUCV='P' and
068400151218     c                             (wbopar=*blanks or wft1<>*blanks)
068500151218     C                   SETON                                        90
068600151218     c                   if        wbopar='S' and wft1='N' and wcca='7'
068700151218     C                   MOVEL     MSG(27)       D66MSG
068800151218     c                   else
068900151218     C                   MOVEL     MSG(3)        D66MSG
069000151218     c                   eval      d66msg=%trim(d66msg)
069100151218     c                             + ' per bolla già in arrivo o consegnata'
069200151218     c                   endif
069300151218     c*- - - - - - - - - - - - - - - -
069400151218     c* cvb utilizzabile in arrivo SOLO su ARB
069500151218     c* --> errore: Causale Variazione utilizzabile solo per bolle presenti in arrivo
069600151218     c*- - - - - - - - - - - - - - - -
069700151218     C                   when      *in01 and §7lUCV=' ' and wbopar='S'
069800151218     C                   SETON                                        90
069900151218     C                   MOVEL     MSG(19)       D66MSG
070000151218     c*- - - - - - - - - - - - - - - -
070100151218     c* cvb utilizzabile in arrivo su ARB Oppure su BLP NON TRASMESSO
070200151218     c* --> errore se bolla BLP già trasmesso
070300151218     c*- - - - - - - - - - - - - - - -
070400151218     C                   when      *in01 and
070500151218     C                             §7lUCV='T' and wbopar='S' and wft1<>*blanks
070600151218     C                   SETON                                        90
070700151218     c* Bolla chiusa in partenza per merce mai affidata
070800151218     c                   if        wft1='N' and wcca='7'
070900151218     C                   MOVEL     MSG(27)       D66MSG
071000151218     c                   else
071100151218     c* bolla non manutenzionabile
071200151218     C                   MOVEL     MSG(26)       D66MSG
071300151218     c                   endif
071400151217     c                   endsl
071500941013    1C                   ENDIF
071600941013     C*
071700941013    1C     D66TBO        IFEQ      'P'
071800941014     C     §7LUCP        ANDEQ     'N'
071900941013     C                   SETON                                        90
072000941013     C                   MOVEL     MSG(3)        D66MSG
072100941013    1C                   ENDIF
072200140502
072300140502     c                   endif
072400941013     C*
072500160119     c                   if        §7lucv='T' or §7lucv='P'
072600151221     c                   eval      d66utp=§7lucv
072700151221     c                   endif
072800160119     c* se causale utilizzabile solo per chiamate cieche abilito alla
072900160119     c* manutenzione di una bolla ancora da partire
073000160119     c                   if        §7lucv='N' and §7lucp='N'
073100160119     c                   eval      d66utp='T'
073200160119     c                   endif
073300151221
073400941013    1C     *IN01         IFEQ      *ON
073500941013     C     *IN90         ANDEQ     *OFF
073600151218     c*
073700941013     C*
073800941013     C* SE GIACENTE ESCLUDO LA VARIAZIONE DI C/A
073900011121      * PER LE BOLLE ARRIVI
074000941013    2C     WGIAC         IFEQ      'S'
074100011121     C     D66TBO        ANDEQ     'A'
074200941013     C     FLGCVB        ANDEQ     'K'
074300011120     C     VIDCOD        ANDNE     'KA'
074400011120     C     VIDCOD        ANDNE     'KP'
074500941013     C                   SETON                                        90
074600941013     C                   MOVEL     MSG(4)        D66MSG
074700941013     C                   GOTO      ENDCCV
074800941013    2C                   ENDIF
074900011121      * PER LE BOLLE PARTENZA
075000011121    2C     WGIAC         IFEQ      'S'
075100011121     C     D66TBO        ANDEQ     'P'
075200011121     C     FLGCVB        ANDEQ     'K'
075300011121     C                   SETON                                        90
075400011121     C                   MOVEL     MSG(4)        D66MSG
075500011121     C                   GOTO      ENDCCV
075600011121    2C                   ENDIF
075700941013     C**
075800941013     C* F L A G   C / A S S E G N O
075900941013     C**
076000941013     C* SE NON C'E' C/A  O GIA' INCASSATO NON POSSO USARE LA KA/KB
076100941013    2C     §7LFCA        IFEQ      'S'
076200941013    3C     §3AFCA        IFEQ      0
076300950119     C     WICC          ORNE      ' '
076400950119     C     WICC          ANDNE     'R'
076500941013     C                   SETON                                        90
076600941013     C                   MOVEL     MSG(5)        D66MSG
076700941013     C                   GOTO      ENDCCV
076800941013    3C                   ENDIF
076900950120     C* SE C'E IL FLAG C/A MA E' BOLLA LEGATA IN LOCALE --> DEVO
077000950120     C*  VARIARE LA MAMMA
077100950120    3C     §3AFCA        IFNE      0
077200950120     C     KBOL          CHAIN     FNLBL01L                           30
077300950120    4C     *IN30         IFEQ      *OFF
077400950120     C     LBLLAP        ANDEQ     LBLLAN
077500950120     C                   SETON                                        90
077600950120     C* PREPARO IL MESSAGGIO
077700950120     C                   MOVEA     MSG(16)       K78
077800950120     C                   MOVE      LBLAAP        W002A             2
077900950120     C                   MOVEA     W002A         K78(59)
078000950120     C                   MOVEL     LBLLPP        W003A             3
078100950120     C                   MOVEA     W003A         K78(62)
078200950120     C                   MOVE      LBLNRP        W002A
078300950120     C                   MOVEA     W002A         K78(66)
078400950120     C                   MOVE      LBLNSP        W007A             7
078500950120     C                   MOVEA     W007A         K78(69)
078600950120     C*
078700950120     C                   MOVEA     K78           D66MSG
078800950120     C                   GOTO      ENDCCV
078900950120    4C                   ENDIF
079000950120    3C                   ENDIF
079100950120    2C                   ENDIF
079200000510     C* PER BOLLE POSTE
079300151217     C**   §OGPT         IFEQ      'S'
079400151217     C**   §7LFPT        ANDEQ     ' '
079500151217     C**                 SETON                                        90
079600151217     C**                 MOVEL     MSG(19)       D66MSG
079700151217     C**                 GOTO      ENDCCV
079800151217    3C**                 ENDIF
079900941013     C**
080000941013     C* T I P O   D I   B O L L A
080100941013     C**
080200941013    2C     §7LVFA        IFEQ      'F'
080300060301     C***  FLGTB1        ANDNE     'F'
080400060301     c                   if        flgtb1 <> 'F'  or
080500060301     c                             (§7lprp =  'S'  and wdft = 0 ) or
080600060301     c                             (§7lprp =  'N'  and wdft > 0 )
080700941013     C                   SETON                                        90
080800060301     c                   endif
080900941013    2C                   ENDIF
081000941013    2C     §7LVFA        IFEQ      'A'
081100941013     C     FLGTB1        ANDNE     'A'
081200941013     C                   SETON                                        90
081300941013    2C                   ENDIF
081400941013    2C     §7LVFA        IFEQ      'E'
081500941013     C     §3ATB2        ANDEQ     *BLANKS
081600941013     C                   SETON                                        90
081700941013    2C                   ENDIF
081800941013     C*
081900941013    2C     *IN90         IFEQ      *ON
082000061025     C                   MOVEL     MSG(13)       D66MSG
082100941013     C                   GOTO      ENDCCV
082200941013    2C                   ENDIF
082300941013     C**
082400941013     C* C A M B I O   C O D I C E   B O L L A
082500941013     C**
082600941013     C* PER TIPI BOLLA CREATI IN AUTOMATICO NON E' POSSIBILE
082700941013     C* SE E' DIROTTAMENTO AD ALTRA FILIALE, SI
082800941013    2C     VIDCOD        IFEQ      'K8'
082900941013     C     VIDCOD        OREQ      'K5'
083000941013     C*
083100941013    3C     §3AUCB        IFEQ      'N'
083200941013     C     WLNP          ANDEQ     WLNA
083300941013     C                   SETON                                        90
083400941013     C                   MOVEL     MSG(6)        D66MSG
083500941013     C                   GOTO      ENDCCV
083600941013    3C                   ENDIF
083700981221     C* SE E' CAMBIO DI PORTO NON POSSO MODIFICARE IL TIPO BOLLA
083800981221    3C     §3AUCB        IFEQ      'N'
083900981221     C     WCCA          ANDEQ     '9'
084000981221     C                   SETON                                        90
084100981221     C                   MOVEL     MSG(18)       D66MSG
084200981221     C                   GOTO      ENDCCV
084300981221    3C                   ENDIF
084400941013    2C                   ENDIF
084500941013     C**
084600941013     C* F L A G   C O N S E G N A
084700941013     C**
084800061025     c* Metto il flag in DS
084900061026     c                   movel     §7lvdco       ds7lcampo
085000061026     c* Imposto il camp data con la data consegna
085100061026     c                   eval      Wdata=Wdcm
085200061026     c                   Exsr      CTRds7lcampo
085300061026     c
085400061026     c                   if        tipo='P' and flag='1'
085500061026     C                   MOVEL     MSG(7)        D66MSG
085600061026     C                   GOTO      ENDCCV
085700061026    2C                   ENDIF
085800061026     c                   if        tipo='D' and flag='1'
085900061026     C                   MOVEL     MSG(8)        D66MSG
086000061026     C                   GOTO      ENDCCV
086100061026    2C                   ENDIF
086200061026     c                   if        tipo='D' and flag='2'
086300061026     C                   MOVEL     MSG(23)       D66MSG
086400061026     C                   GOTO      ENDCCV
086500061026    2C                   ENDIF
086600061026     c
086700941013     C* NON VARIABILE DOPO CONSEGNA
086800061025    2C***  §7LVDC        IFEQ      ' '
086900061025     C***  §7LVDC        OREQ      'N'
087000061025    3C***  WDCM          IFGT      0
087100061025     C***                SETON                                        90
087200061025     C***                MOVEL     MSG(7)        D66MSG
087300061025     C***                GOTO      ENDCCV
087400061025    3C***                ENDIF
087500061025    2C***                ENDIF
087600061025     C***
087700941013     C* VARIABILE SOLO DOPO LA CONSEGNA
087800061025    2C***  §7LVDC        IFEQ      'C'
087900061025     C***  WDCM          ANDEQ     0
088000061025     C***                SETON                                        90
088100061025     C***                MOVEL     MSG(8)        D66MSG
088200061025     C***                GOTO      ENDCCV
088300061025    2C***                ENDIF
088400941013     C**
088500941013     C* F L A G   C O N S E G N A   P A R Z I A L E
088600941013     C**
088700061026     C**
088800061102     c* lo controllo solo se la data consegna = 0 altrimenti non lo faccio
088900061102     c                   if        wdcm=0
089000061026     c* Metto il flag in DS
089100061026     c                   movel     §7lvdcop      ds7lcampo
089200061026     c* Imposto il camp data con la data consegna
089300061026     c                   eval      Wdata=Wdcp
089400061026     c                   Exsr      CTRds7lcampo
089500061026     c
089600061026     c                   if        tipo='P' and flag='1'
089700061026     C                   MOVEL     MSG(9)        D66MSG
089800061026     C                   GOTO      ENDCCV
089900061026    2C                   ENDIF
090000061026     c                   if        tipo='D' and flag='1'
090100061026     C                   MOVEL     MSG(8)        D66MSG
090200061026     C                   GOTO      ENDCCV
090300061026    2C                   ENDIF
090400061026     c                   if        tipo='D' and flag='2'
090500061026     C                   MOVEL     MSG(24)       D66MSG
090600061026     C                   GOTO      ENDCCV
090700061026    2C                   ENDIF
090800061102    2C                   ENDIF
090900941013     C* NON VARIABILE DOPO CONSEGNA PARZIALE
091000061026    2C**   §7LVDP        IFEQ      ' '
091100061026     C**   WDCP          ANDGT     0
091200061026     C**                 SETON                                        90
091300061026     C**                 MOVEL     MSG(9)        D66MSG
091400061026     C**                 GOTO      ENDCCV
091500061026    2C**                 ENDIF
091600941013     C*
091700941013     C* VARIABILE SOLO DOPO LA CONSEGNA PARZIALE
091800061026    2C**   §7LVDP        IFEQ      'C'
091900061026     C**   WDCP          ANDEQ     0
092000061026     C**                 SETON                                        90
092100061026     C**                 MOVEL     MSG(8)        D66MSG
092200061026     C**                 GOTO      ENDCCV
092300061026    2C**                 ENDIF
092400941013     C**
092500941013     C* F L A G   C O N T A B I L I Z Z A Z I O N E
092600941013     C**
092700941013     C* PRENDO LA DATA FATTURA DELLA 2 BOLLE SE
092800941013     C*  1) CAUSALE INIZIA PER 'K'
092900941013     C*  2) SE E' PROPRIO PER 2 BOLLA
093000941013     C*  3) SE E' VARIAZIONE DEL DESTINATARIO
093100941013    2C     §7LVFA        IFEQ      'E'
093200941013     C*
093300941013     C     §3ATB2        ORNE      *BLANKS
093400941013     C     FLGCVB        ANDEQ     'K'
093500941013     C*
093600950301     C     §3ATB2        ORNE      *BLANKS
093700950301     C     VIDCOD        ANDEQ     'I0'
093800000104     C                   MOVEL     W2DFT         WDFTC             8 0
093900941013   X2C                   ELSE
094000941013     C                   MOVEL     WDFT          WDFTC
094100941013    2C                   ENDIF
094200941013     C*
094300941013     C* NON VARIABILE DOPO LA CONTABILIZZAZIONE
094400941013    2C     WDFTC         IFGT      0
094500941013     C     §7LVDF        ANDEQ     ' '
094600000104     C     WDFTC         ANDLE     WDTU10
094700941013     C                   SETON                                        90
094800941013     C                   MOVEL     MSG(11)       D66MSG
094900941013     C                   GOTO      ENDCCV
095000941013    2C                   ENDIF
095100941013     C*
095200941013     C* VARIABILE SOLO DOPO LA CONTABILIZZAZIONE
095300941013    2C     §7LVDF        IFEQ      'C'
095400000104    3C     WDFTC         IFGT      WDTU10
095500941013     C     WDFTC         OREQ      0
095600941013     C                   SETON                                        90
095700941013     C                   MOVEL     MSG(12)       D66MSG
095800941013     C                   GOTO      ENDCCV
095900941013    3C                   ENDIF
096000941013    2C                   ENDIF
096100130410     c* Non variabile dopo contabilizzazione/incasso per fatture immediate
096200130405     c                   exsr      opnfile
096300130614     c***                if        §7lbfi='S' or §7lvdf=' '
096400130410     c* Setll su Titas senza tipo bolla e se bolla non esiste
096500130410     c* sicuramente non è contabilizzata e quindi permetto la variazione
096600130409     c     kbol          setll     titas30c
096700130409     c                   if        %equal(titas30c)
096800130409     c                   clear                   ktbl
096900130409     c                   select
097000130409     c* bolla variabile: franco o assegnato chaino con tipo bolla §3atb1
097100130409     c                   when      §7lvfa='F' or §7lvfa='A'
097200130405     c                   eval      ktbl=§3atb1
097300130409     c                   exsr      chtitas
097400130409     c   90              goto      endccv
097500130409     c* bolla variabile: estensione chaino con tipo bolla §3atb2
097600130409     c                   when      §7lvfa='E'
097700130409     c                   eval      ktbl=§3atb2
097800130409     c                   exsr      chtitas
097900130409     c   90              goto      endccv
098000130409     c                   other
098100130409     c* bolla variabile: tutte chaino con §3atb1 e §3atb2 se presente
098200130409     c                   eval      ktbl=§3atb1
098300130409     c                   exsr      chtitas
098400130409     c   90              goto      endccv
098500130409     c                   if        §3atb2<>*blanks
098600130409     c                   eval      ktbl=§3atb2
098700130409     c                   exsr      chtitas
098800130409     c   90              goto      endccv
098900130409     c                   endif
099000130409     c                   endsl
099100130409     c
099200130409     c                   endif
099300130614     c***                endif
099400941013     C**
099500941013     C* P E R   B O L L A     S E G U E   F A T T U R A
099600941013     C**
099700941013     C     §7LBSF        IFEQ      'S'
099800941013     C     WDFTC         ANDGT     0
099900941013     C                   SETON                                        90
100000941013     C                   MOVEL     MSG(14)       D66MSG
100100941013     C                   GOTO      ENDCCV
100200941013     C                   ENDIF
100300941013     C**
100400941013     C* P E R   B O L L A   F A T T U R A   I M M E D I A T A
100500941013     C**
100600941013     C     §7LBFI        IFEQ      'S'
100700941013     C     WDFTC         ANDEQ     0
100800941013     C                   SETON                                        90
100900941013     C                   MOVEL     MSG(15)       D66MSG
101000941013     C                   GOTO      ENDCCV
101100941013     C                   ENDIF
101200070305     c
101300070305     c* Non faccio utilizzare se cpdioce bollo storno assegnato
101400070305     c                   if        §7lBFI='S' or §7lBSF='S'
101500070305     c                   if        (§7lvfa='A' and §3atb1='AS') or
101600070305     c                             (§7lvfa='E' and §3atb2='AP')
101700070305     C                   SETON                                        90
101800070305     C                   MOVEL     MSG(25)       D66MSG
101900070305     C                   GOTO      ENDCCV
102000070305     C                   ENDIF
102100070305     c                   endif
102200070305     c
102300950331     C* SE GIA INCASSATA NON SI PUO' UTILIZZARE LA CAUSALE PER
102400990415     C* FATTURE IMMEDIATE SE LA CAUASALE NON LO PERMETTE
102500130410     C***  §7LBFI        IFEQ      'S'
102600130410     C***  §7LFIN        ANDNE     'S'
102700130410     C***  ARBICA        ifne      ' '
102800130410     C***  ARBICA        ANDNE     'R'
102900130410     C***                SETON                                        90
103000130410     C***                MOVEL     MSG(17)       D66MSG
103100130410     C***                GOTO      ENDCCV
103200130410     c***                endif
103300130410     c***  arbica        ifeq      'R'
103400130410     C***  KARI          CHAIN     FIARI01L
103500130410     c***                if        %found(fiari01l) and ariatb=' '
103600130410     C***                SETON                                        90
103700130410     C***                MOVEL     MSG(17)       D66MSG
103800130410     C***                GOTO      ENDCCV
103900130410     c***                endif
104000130410     c****               endif
104100130410     C****               ENDIF
104200060302     c* per bolle partenze (prepagati)
104300060303     c                   if        §7lvfa = 'F' and §7lprp = 'S'
104400060302     c                   z-add     blpaas        prmaas
104500060302     c                   z-add     blplnp        prmlnp
104600060302     c                   z-add     blpnrs        prmnrs
104700060302     c                   z-add     blpnsp        prmnsp
104800060302     c                   z-add     ar6fiv        prmfiv
104900060302     c                   z-add     ar6nft        prmnft
105000060302     c                   z-add     wdft          prmdft
105100060302     c                   clear                   prmesito
105200060608     c                   call      'YCOW0121R1'
105300060302     C                   PARM                    prmaas
105400060302     C                   PARM                    prmLnp
105500060302     C                   PARM                    prmNrs
105600060302     C                   PARM                    prmNsp
105700060302     C                   PARM                    prmFiv
105800060302     C                   PARM                    prmDft
105900060302     C                   PARM                    prmNft
106000060302     C                   PARM                    prmEsito
106100060302     c                   if        prmesito <> 0
106200060302     C                   MOVEL     msg(17)       d66msg
106300060302     C                   SETON                                        90
106400060302     C                   GOTO      endccv
106500060302     c                   endif
106600060302     c                   endif
106700950331     C* SE BOLLA IN ASSEGNATO MA NON ANCORA ABILITATO, NON POOSO UTILIZ
106800941013     C*  UNA CAUSALE DI TASSAZIONE RITASSAZIONE BOLLA
106900060303     C     ARBACA        ifeq      ' '
107000941013     C     §7LRTA        IFEQ      'S'
107100941013     C     §7LBFI        OREQ      'S'
107200941013     C     §7LBSF        OREQ      'S'
107300941013     C                   SETON                                        90
107400941013     C                   MOVEL     MSG(10)       D66MSG
107500941013     C                   GOTO      ENDCCV
107600941013     C                   ENDIF
107700941013     C                   ENDIF
107800021128      **
107900021128      * C O N S E G N A   P A R T I C O L A R E
108000021128      **
108100021128      * Se presente la  particolare "Supermercati" la causale deve
108200021128      * essere abilitata
108300021128     c                   If         §7lSup = 'N' and
108400021128     c                             (Wtc1 = 'S' or Wtc2 = 'S')
108500021128     c                   Eval      *In90 = *On
108600021128     c                   Eval      D66Msg = Msg(20)
108700021128     c                   GoTo      EndCcv
108800021128     c                   endif
108900941013     C*
109000941013     C                   ENDIF
109100941013     C*
109200941013     C     ENDCCV        ENDSR
109300061026     c*
109400061026     c* controllo DS7Lcampo  ---------------------------------------------*
109500061026     c     CTRds7lcampo  BEGSR
109600061026     c                   clear                   tipo
109700061026     c                   clear                   flag
109800061026     c* 1° flag --> §7LVDOPO variabile prima o dopo
109900061026     c*             Non variabile DOPO
110000061026    2c                   if        (§7lvdopo =' ' or §7lvdopo='N') and
110100061026     c                             wdata>0
110200061026     c                   movel     '1'           flag
110300061026     c                   movel     'P'           tipo
110400061026     c                   seton                                        90
110500061026     C                   GOTO      ENDctr
110600061026    2C                   ENDIF
110700061026     c
110800061026     c*             Variabile SOLO DOPO
110900061026    2c                   if        §7lvdopo ='C' and wdata=0
111000061026     c                   movel     '1'           flag
111100061026     c                   movel     'D'           tipo
111200061026     C                   SETON                                        90
111300061026     C                   GOTO      ENDctr
111400061026    2C                   ENDIF
111500061108     c
111600061108     c                   if        wdata>0
111700061026     c*
111800061026     c* 2° flag --> se pieno Include o ESclude alcuni tipi bolla
111900061026     c                   setoff                                       90
112000061108     c
112100061026     c* 3° flag --> tipo bolla da includere/escludere
112200061026    2c                   if        §7lines='I'
112300061026    3c                   if        §7lfras='F'  and flgtb1<>'F'
112400061026     C                   SETON                                        90
112500061026    3c                   endif
112600061026    3c                   if        §7lfras='A'  and flgtb1<>'A'
112700061026     C                   SETON                                        90
112800061026    3c                   endif
112900061026    3c                   if        §7lfras='E'  and §3atb2='  '
113000061026     C                   SETON                                        90
113100061026    3c                   endif
113200061026    3c                   if        §7lfras='S'  and §3atb2='  '
113300061026     c                             and flgtb1='F'
113400061026     C                   SETON                                        90
113500061026    3c                   endif
113600061026     c
113700061026     c* 4° flag -->  includi codificati o non
113800061026    3c                   if        not *in90 and §7lcodif='C'
113900061026     c                   EXSR      Codificato
114000061026    3c                   endif
114100061026     c
114200061026    3c                   if        not *in90 and §7lcodif='N'
114300061026     c                   exsr      NonCodificato
114400061026    3c                   endif
114500061026     c
114600061026    2c                   endif
114700061026    c
114800061026    2c                   if        §7lines='E'
114900061026    3c                   if        §7lfras='F'  and flgtb1='F'
115000061026     C                   SETON                                        90
115100061026    3c                   endif
115200061026    3c                   if        §7lfras='A'  and flgtb1='A'
115300061026     C                   SETON                                        90
115400061026    3c                   endif
115500061026    3c                   if        §7lfras='E'  and §3atb2<>'  '
115600061026     C                   SETON                                        90
115700061026    3c                   endif
115800061026    3c                   if        §7lfras='S'  and (§3atb2<>'  '
115900061026     c                             or  flgtb1='A')
116000061026     C                   SETON                                        90
116100061026    3c                   endif
116200061026    c
116300061026     c* 4° flag --> esclcudi codificati o non
116400061026    3c                   if        not *in90 and §7lcodif='C'
116500061026     c* Devo escludere i non codificati
116600061026     c                   exsr      NonCodificato
116700061026    3c                   endif
116800061026     c
116900061026    3c                   if        not *in90 and §7lcodif='N'
117000061026     c                   exsr      COdificato
117100061026    3c                   endif
117200061026     c
117300061026    2c                   endif
117400061026     C*
117500061026    2C     *IN90         IFEQ      *ON
117600061026     c                   movel     '2'           flag
117700061026     c                   movel     'D'           tipo
117800061026     C                   GOTO      ENDctr
117900061026    2C                   ENDIF
118000061108    2C                   ENDIF
118100061026     c     ENDCTR        ENDSR
118200061026     c
118300061026     c* OK se bolla     codificata ----------------------------------*
118400061026     c     Codificato    BEGSR
118500061026     c*  di 1°bolla
118600061026    4c                   if        §7lfras<>'E' and (wfineksc1=8888 or
118700061026     c                                              wfineksc1=9999)
118800061026     c                   seton                                        90
118900061026    4c                   endif
119000061026     c*  di 2°bolla
119100061026    4c                   if        (§7lfras='E' or §7lfras='S' or
119200061026    4c                              §7lfras=' ') and
119300061026     c                             wfineksc2=9999
119400061026     c                   seton                                        90
119500061026    4c                   endif
119600061026     c                   ENDSR
119700061026     c
119800061026     c* OK se bolla non codificata -----------------------------------*
119900061026     c     nonCodificato begsr
120000061026     c*  di 1°bolla
120100061026    4c                   if        §7lfras<>'E' and (wfineksc1<>8888 and
120200061026     c                                              wfineksc1<>9999)
120300061026     c                   seton                                        90
120400061026    4c                   endif
120500061026     c*  di 2°bolla
120600061026    4c                   if        (§7lfras='E' or §7lfras='S' or
120700061026    4c                              §7lfras=' ') and
120800061026     c                             wfineksc2<>9999 and wfineksc2>0
120900061026     c                   seton                                        90
121000061026    4c                   endif
121100061026     c                   ENDSR
121200061026     c*
121300061026     c* Decodifica la DS7Lcampo ------------------------------------------*
121400061026     c     Decodi7LCAMPO BEGSR
121500061026     c                   clear                   des28
121600061031     c                   if        §7lines<>' '
121700061031     c
121800061026     c                   if        §7lines='I'
121900061026     c                   movel     'Includi'     des28
122000061026     c                   endif
122100061026     c                   if        §7lines='E'
122200061026     c                   movel     'Escludi'     des28
122300061026     c                   endif
122400061026     c
122500061026     c                   select
122600061026     c                   when      §7lfras='F'
122700061031     c                   eval      des28=%trim(des28)+' Franchi '
122800061026     c                   when      §7lfras='A'
122900061031     c                   eval      des28=%trim(des28)+' Asseg.1Bol'
123000061026     c                   when      §7lfras='E'
123100061031     c                   eval      des28=%trim(des28)+' Asseg.2Bol'
123200061026     c                   when      §7lfras='S'
123300061031     c                   eval      des28=%trim(des28)+' Asseg1/2Bo'
123400061026     c                   when      §7lfras=' '
123500061031     c                   eval      des28=%trim(des28)+' TutteBolle '
123600061026     c                   endsl
123700061026     c
123800061026     c                   if        §7lcodif='C'
123900061031     c                   eval      des28=%trim(des28)+' Codific'
124000061026     c                   endif
124100061026     c                   if        §7lcodif='N'
124200061031     c                   eval      des28=%trim(des28)+' NoCodif'
124300061026     c                   endif
124400061031     c                   endif
124500061026     c                   ENDSR
124600941013     C*
124700060302     C* CONtROLLO SE EFFETTUATA SELEZIONE ----------------------------*
124800920323     C     CONTR         BEGSR
124900941013     C                   READC     LR66DSF                                31
125000920323     C*
125100941013     C     *IN31         DOWEQ     *OFF
125200941013     C*
125300920323     C     VIDSEL        IFEQ      '1'
125400941013     C                   MOVEL     VIDCOD        D66CVB
125500941013     C                   MOVEL     DESCOD        D66DES
125600941013     C                   MOVEL     FLGFFI        D66FFI
125700941013     C                   MOVEL     FLGC4V        D66C4V
125800941013     C                   MOVEL     FLGSTF        D66STF
125900060130     C                   MOVEL     FLGPOCL       D66POCL
126000941013     C                   MOVEL     FLGFSA        D66FSA
126100941013     C                   MOVEL     FLGRTA        D66RTA
126200060130     C                   MOVEL     FLGMTV        D66MTV
126300920323     C                   SETON                                        31
126400941013     C                   ENDIF
126500920323     C*
126600941013     C  N31              READC     LR66DSF                                31
126700941013     C                   ENDDO
126800920323     C                   ENDSR
126900130405     C* Apertura titas -----------------------------------------------*
127000130405     C     opnfile       BEGSR
127100130405     c                   if        not %open(titas30c)
127200130405     c                   if        %subst(knsif:7:1)='P'
127300130405     c                   eval      wlibsede=wlibsedep
127400130405     c                   else
127500130405     c                   eval      wlibsede=wlibsedeb
127600130405     c                   endif
127700130405     c                   open      titas30c
127800130405     c                   endif
127900130405     c                   endsr
128000130409     C* Chain titas  per controllo se contabilizzata -----------------*
128100130409     C     chtitas       BEGSR
128200130418     c                   eval      ktbl1=%subst(ktbl:1:1)
128300130418     c     ktas          setll     titas30c
128400130418     c     ktas1         reade     titas30c
128500130419    1c                   if        not %eof(titas30c) and
128600130419     c                             %subst(tastbl:1:1)=ktbl1
128700130418
128800130410     c* BOLLA CONTABILIZZATA
128900130410     c* --> sempre errore se non variabile dopo contabilizzazione
129000130410    2c                   if        tasfic<>*blanks and §7lvdf=*blanks
129100130409     c                   Movel     Msg(11)       D66Msg
129200130409     c                   Eval      *In90 = *On
129300130410   x2c                   else
129400130410     c* BOLLA NON CONTABILIZZATA oppure VARIABILE DOPO CONTABILIZZ.
129500130410     c* Se variazione di tassazione
129600130415    3c                   if        %subst(vidcod:1:1) = 'T'
129700130410     c* Bolla     Contabilizzata: si può variare solo ias e vmd
129800130410    4c                   if        tasfic<>*blanks
129900130410     c                   eval      d66bdi='S'
130000130410   x4c                   else
130100130410     c* Bolla non Contabilizzata: controllo incasso e se già incassata
130200130410     c* si può variare solo IAS e VMD
130300130410    5c                   if        §7lbfi='S'
130400130410    6c                   if        arbica<>' ' and arbica <>'R'
130500130410    7c                   if        §7lvdf='S'
130600130409     c                   eval      d66bdi='S'
130700130410   x7c                   else
130800130410     c* errore se non variabile dopo incasso
130900130410     c                   Movel     Msg(17)       D66Msg
131000130410     c                   Eval      *In90 = *On
131100130410    7c                   endif
131200130410    6c                   endif
131300130410    6c                   if        arbica='R'
131400130409     C     KARI          CHAIN     FIARI01L
131500130410    7c                   if        %found(fiari01l) and ariatb=' '
131600130410    8c                   if        §7lvdf='S'
131700130409     c                   eval      d66bdi='S'
131800130410     c* errore se non variabile dopo incasso
131900130410     c                   Movel     Msg(17)       D66Msg
132000130410     c                   Eval      *In90 = *On
132100130410   x8c                   else
132200130410     c                   endif
132300130410    7c                   endif
132400130410    6c                   endif
132500130410    5c                   endif
132600130410    4c                   endif
132700130409    3c                   endif
132800130409    2c                   endif
132900130409    1c                   endif
133000130409     c                   endsr
133100000510**
133200941013Bolla inesistente                                                              1
133300941013Causale Variazione inesistente                                                 2
133400151218Causale Variazione non utilizzabile                                            3
133500941013Causale non utilizzabile: spedizione giacente.Variare eseguendo le disposizion 4
133600950131Causale Variazione per C/Assegno ma la bolla e' senza C/A o e' gia' incassato  5
133700941013Il codice bolla non puo' essere modificato per una spedizione locale           6
133800941013Causale non utilizzabile: Spedizione gia' consegnata                           7
133900941013Causale utilizzabile soltanto dopo la consegna completa o parziale, della bolla8
134000941013Causale non utilizzabile: e' gia' stata eseguita una consegna parziale         9
134100941013Per eseguire questo tipo di variazione tassare prima la bolla                 10
134200941013Causale non utilizzabile: spedizione gia' contabilizzata                      11
134300941013Causale utilizzabile soltanto dopo la contabilizzazione della bolla           12
134400941013Causale non utilizzabile per il tipo di bolla richiesto                       13
134500941013Causale utilizzabile soltanto per bolle Segue Fattura                         14
134600941013Causale utilizzabile soltanto per bolle con fattura immediata                 15
134700950120BOLLA LEGATA:il C/Assegno e' da variare sulla bolla MAMMA xx xxx xx xxxxxxx   16
134800950331Causale non utilizzabile: Spedizione gia' incassata                           17
134900981221Per i Cambi di Porto impossibile rimodificare il tipo bolla della bolla mamma
135000151218Causale Variazione utilizzabile solo per bolle presenti in arrivo             19
135100021128Causale non utilizzabile: spedizione con consegna particolare "SUPERMERCATI"  20
135200030127Causale non utilizzabile: spedizione senza bancali                            21
135300030127Causale non utilizzabile: variazione già effettuata                           22
135400061025Causale non utilizzabile dopo CONSEGNA per il tipo di bolla richiesto         23
135500061026Causale non utilizzabile dopo CONSEGNA PARZIALE per il tipo di bolla richiesto
135600070305Causale non utilizzabile: spedizione stornata (non viene fatturata)           25
135700151218Bolla non manutenzionabile per questa Causale Variazione                      26
135800151218Bolla non manutenzionabile: chiusa in partenza per "Merce mai Affidata"       27
