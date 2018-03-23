000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500030911     FTELE2XLS5 uF A E             DISK
000600030812     FTNVASWWT  O    E             DISK
000700030409     FTITAS30C  IF   E           K DISK
000800030409     FTNCSB03L  IF   E           K DISK
000900030409     FTIVIR01L  UF   E           K DISK
001000030813     FTIVLT01L  UF A E           K DISK
001100030813     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001200030813     F                                     PREFIX(f_)
001300030813     F                                     USROPN
001400991027
001500030429     D prmppt          s             50
001600991027     D prmesito        s              1
001700030813     D prmksc          s              7
001800030813     D prmksc2         s              7
001900991027     D wrkesito        s                   like(prmesito)
002000030409     D flgIntXLS5      s              1    inz('0')
002100030911     D wInt5           s              1    inz('N')
002200030417     D wParmGgSet      s              1  0 inz(*zeros)
002300030813     D wISV            s              2    INZ(*blanks)
002400030813     D dwlprg          s             10    INZ(*all'0')
002500030813     D wrkprg          s              8  0 INZ(*zeros)
002600030813     D wrkoggiiso      s               d
002700030813     D wrkmese         s              2  0
002800030813     D                 ds                  INZ
002900030813     D wrkday                  1      2s 0
003000030813     D wrkdaychr                      2    overlay(wrkday)
003100030813
003200090122     D tnvasds       e ds                  extname(tnvas00t)
003300030429     D dsvas         e ds                  extname(tnvas00t)
003400030409     D X16GGSETDS    e ds
003500030813     D mc              s              1    dim(12) ctdata perrcd(12)
003600030409
003700030409     D*---------------------
003800030409     D* DS "XSRDA8" - CONTROLLA DATA (8)
003900030409     D*---------------------
004000030409     D WLBDA8          DS
004100030409     D  G08DAT                 1      8  0
004200030409     D  G08INV                 9     16  0
004300030409     D  G08ERR                17     17
004400030409     D  G08TGI                18     22  0
004500030409
004600030409
004700030813
004800030813      /TITLE MAIN
004900030813     C*
005000030813     C* Se trattasi di cliente 0052454 effettuo le considerazioni qui sotto, se trattasi invece
005100030813     C* di cliente 0052454 invece elaboro sempre cmq
005200030813     C                   IF        prmksc = '0052454'
005300030417     C*
005400030417     C* Verifico se ricevuti dei parametri (nella fatispecie il giorno della settimana in cui
005500030417     C* effettuare la traduzione
005600030417     C                   EVAL      wParmGgSet = 2                               * DFT = Lunedì
005700030417     C                   IF        %subst(prmppt:9:1) <> *blanks
005800030417     C                   EVAL      PiStr=%trim(%subst(prmppt:9:1))
005900030417     C                   EXSR      CHKNUM
006000030417     C                   IF        PiInt=*on
006100030417     C                   Z-ADD     PiVal         wParmGgSet
006200030417     C                   ENDIF
006300030417     C                   ENDIF
006400030409     C*
006500030409     C* Innanzitutto verifico se la traduzione è da eseguire in quanto il cliente vuole i dati
006600030409     C* solamente 1 volta a settimana (nella fattispecie abbiamo stabilito il lunedì)
006700030409     C                   CLEAR                   X16GGSETDS
006800030409     C     *ISO          MOVE      DATCOR        X16DATA
006900030409     C                   CALL      'X16GGSET'
007000030409     C                   PARM                    X16GGSETDS
007100030417     C                   IF        X16GIORNO = wParmGgSet                          * Giorno settim.
007200030409     C                   EVAL      virKSC = '00052454'
007300030429 xxx C                   EVAL      virSEQ = 006                                    * FONDAMENTALE *
007400030409     C     KEYvir01      CHAIN     TIVIR01L
007500030409     C                   IF        %found(TIVIR01L)
007600030417     C                   IF        %subst(virPPT:1:8) <>
007700030417     C                             %editw(DATCOR:'        ')
007800030911     C                   EXSR      chkRECFILMBR
007900030409     C                   EXSR      traduci
008000030417     C                   EVAL      %subst(virPPT:1:8) =
008100030417     C                             %editw(DATCOR:'        ')
008200030409     C                   UPDATE    TIVIR000
008300030409     C                   ENDIF
008400030409     C                   ENDIF
008500030409     C                   ENDIF
008600030813     C*
008700030813     C                   ELSE
008800030813     C*
008900030813     C* Forzo x sicurezza il test che si tratti del cliente 0052552
009000030813     C                   IF        prmksc = '0052552'
009100030911     C                   EXSR      chkRECFILMBR
009200030813     C                   EXSR      traduci
009300030813     C                   EXSR      crealogs
009400030813     C                   ENDIF
009500030813     C                   ENDIF
009600030409     C*
009700030409     C                   EVAL      wrkesito = '0'
009800030409     C*
009900921023     C                   SETON                                        LR
010000030409
010100030911
010200030911
010300030911     C     chkRECFILMBR  BEGSR
010400030911     C*
010500030911     C                   READ      TELE2XLS5                              55
010600030911     C                   IF        *IN55 = *ON
010700030911     C                   MOVEL     'S'           wInt5
010800030911     C                   ENDIF
010900030911     C*
011000030911     C                   ENDSR
011100030409
011200030409
011300991027
011400991027     C     traduci       BEGSR
011500991027     C*
011600090122     C                   READ      tivgdtmp
011700090122     C                   DOW       not %eof(tivgdtmp)
011800090122     C                   movel     vgddta        tnvasds
011900040223     C*
012000040223     C* Se trattasi del cliente SIRTI (0052552) lo do anche a SIRTI (2120004)
012100040223     C                   IF        prmksc = '0052552'
012200040223     C                   EXSR      TRADVAS
012300040223     C                   ENDIF
012400040223     C*
012500030813     C                   MOVEL     'N'           FlgTELE2          1
012600030429     C*
012700030429     C                   EXSR      TRADXLS5
012800030813     C*
012900090122     C                   DELETE    tivgd000
013000991027     C*
013100090122     C                   READ      tivgdtmp
013200030325     C                   ENDDO
013300991027     C*
013400910830     C                   ENDSR
013500030409
013600030409
013700030409
013800030429     C     TRADXLS5      BEGSR
013900030409     C*
014000030409     C* Qualora nn ancora fatto imposto la prima riga con le intestazioni dei campi
014100030911     C                   IF        flgIntXLS5 = '0' AND wInt5 = 'S'
014200030409     C                   EVAL      T25XDTA = '"NUMERO ORDINE"'+','+
014300030410     C                                       '"CODICE CLIENTE TELE2"'+','+
014400030409     C                                       '"RAGIONE SOCIALE"'+','+
014500030409     C                                       '"INDIRIZZO"'+','+
014600030409     C                                       '"LOCALITA"'+','+
014700030409     C                                       '"PROVINCIA"'+','+
014800030409     C                                       '"CAP"'+','+
014900030410     C                                       '"DATA PAGAMENTO"'+','+
015000030409     C                                       '"IMPORTO CONTRASSEGNO"'+','+
015100030409     C                                       '"TIPO INCASSO"'
015200030409     C                   WRITE     TELE2X05
015300030409     C                   EVAL      flgIntXLS5 = '1'
015400030409     C                   ENDIF
015500030409     C*
015600030409     C* "Chaino" la bolla x reperire i dati mancanti
015700030409     C     KEYtas30cP    CHAIN     TITAS30C
015800030409     C                   IF        %found(TITAS30C)
015900030409     C*
016000030409     C* "Chaino" il contrassegno x reperire i dati mancanti
016100030409     C     KEYcsb03      CHAIN     TNCSB03L
016200030409     C                   IF        %found(TNCSB03L)
016300030409     C*
016400030409     C* Reperisco la descrizione del tipo incasso contrassegno
016500030429     C                   IF        VAStpi = 'M'
016600030429     C                   MOVEL(P)  'Mittente'    WdesTPI          10
016700030429     C                   ELSE
016800030429     C                   MOVEL(P)  'Bartolini'   WdesTPI
016900030429     C                   ENDIF
017000030409     C*
017100030409     C* Scrivo i record di dettaglio esiti
017200030429     C                   EVAL      T25XDTA = %trim(%editc(VASRMN:'Q'))+','+
017300030429     C                                          '"'+%trim(VASRMA)+'"'+','+
017400030429     C                                          '"'+%trim(TASRSD)+'"'+','+
017500030409     C                                          '"'+%trim(TASIND)+'"'+','+
017600030409     C                                          '"'+%trim(TASLOD)+'"'+','+
017700030409     C                                          '"'+%trim(TASPRD)+'"'+','+
017800030409     C                                          '"'+%trim(TASCAD)+'"'+','+
017900030409     C                                       %trim(%editc(CSBDDP:'Q'))+','+
018000030430     C                                       %trim(%editc(VASCAS:'P'))+','+
018100030429     C                                          '"'+%trim(WdesTPI)+'"'
018200030409     C*
018300030813     C                   MOVEL     'S'           FlgTELE2
018400030409     C                   WRITE     TELE2X05
018500030409     C*
018600030409     C                   ENDIF
018700030409     C                   ENDIF
018800030409     C*
018900030409     C                   ENDSR
019000030417
019100030813
019200030813     C     TRADVAS       BEGSR
019300030813     C*
019400030813     C                   MOVE      VASLNP        VXSLNP
019500030813     C                   MOVE      VASLNA        VXSLNA
019600030813     C                   MOVE      VASAAS        VXSAAS
019700030813     C                   MOVE      VASNRS        VXSNRS
019800030813     C                   MOVE      VASNSP        VXSNSP
019900030813     C                   MOVE      VASRMN        VXSRMN
020000030813     C                   MOVEL     VASCAS        W11              11            METTO
020100030813     C                   MOVE      ','           W11                            LA ,
020200030813     C                   MOVE      W11           W12              12            NEL C/A
020300030813     C                   MOVE      VASCAS        VXSCAS
020400030813     C                   MOVEL     W12           VXSCAS
020500030813     C                   MOVE      VASVCA        VXSVCA
020600030813     C                   MOVE      VASNRA        VXSNRA
020700030813     C                   MOVE      VASDTE        VXSDTE
020800030813     C                   MOVE      VASTPI        VXSTPI
020900030813     C                   MOVE      VASABI        VXSABI
021000030813     C                   MOVE      VASCAI        VXSCAI
021100030813     C                   MOVE      VASNDP        VXSNDP
021200030813     C                   MOVE      VASDDP        VXSDDP
021300030813     C                   MOVE      VASCCB        VXSCCB
021400030813     C                   MOVE      VASABP        VXSABP
021500030813     C                   MOVE      VASCAP        VXSCAP
021600030813     C                   MOVE      VASFPC        VXSFPC
021700030813     C                   MOVE      VASDVA        VXSDVA
021800030813     C                   MOVE      VASKSC        VXSKSC
021900030813     C                   MOVE      VASRMA        VXSRMA
022000030813     C*
022100030813     C                   WRITE     TNVXS000
022200030813     C*
022300030813     C                   ENDSR
022400030813
022500030813
022600030813
022700030813      /TITLE Valorizzazione Progressivo Applicazione
022800030813     C     calprog       begsr
022900030813     C*
023000030813     C                   open      tis7prgf
023100030813     C*
023200030813     C                   read(e)   tis7prgf
023300030813     C                   if        not %error
023400030813     C                   eval      dwlprg = f_tis7prgf
023500030813     C*
023600030813     C                   move(p)   dwlprg        wrkprg
023700030813     C                   add       1             wrkprg
023800030813     C                   move(p)   wrkprg        dwlprg
023900030813     C                   movel     wISV          dwlprg
024000030813     C*
024100030813     C                   eval      f_tis7prgf = dwlprg
024200030813     C                   update    tis7prg0
024300030813     C                   endif
024400030813     C*
024500030813     C                   close     tis7prgf
024600030813     C*
024700030813     C                   endsr
024800030813
024900030813
025000030813
025100030813
025200030813     C     CREALOGS      BEGSR
025300030813     C*
025400030813     C* Valorizzo i campi comuni a tutti i reord di log da "clonare" (solo x cliente SIRTI: 0052552)
025500030813     C*
025600030813     C                   CLEAR                   TIVLT000
025700030813     C                   EVAL      wISV = 'OB'
025800030813     C                   EVAL      vltKSC = '0' + prmksc2
025900030813     C                   EVAL      vltKSU = '0' + prmksc2
026000030813     C                   EVAL      vltMBD = 'M0'+ prmksc2
026100030813     C                   EVAL      vltSND = 'D'
026200030813     C                   EVAL      vltFLS = 'TNVAS00S'
026300030813     C*
026400030813     C                   time                    wrkoggiiso
026500030813     C                   extrct    wrkoggiiso:*m wrkmese
026600030813     C                   extrct    wrkoggiiso:*d wrkday
026700030813     C                   eval      vltMBS =
026800030813     C                             mc(wrkmese)
026900030813     C                             + wrkdaychr
027000030813     C                             + %subst(vltKSU:2:7)
027100030813     C*
027200030813     C                   EVAL      vltDAT = DATCOR
027300030813     C                   EVAL      vltORA = ORACOR
027400030813     C                   EVAL      vltISV = wISV
027500030813     C                   EVAL      vltTIA = 'F'
027600030813     C                   EVAL      vltFL2 = '01'
027700030813     C                   EVAL      vltSTS = '0'
027800030813     C                   exsr      calprog
027900030813     C                   EVAL      vltPRG = dwlprg
028000030813     C*
028100030813     C                   EVAL      vltTIP = 'VS'
028200030813     C                   EVAL      vltFLD = 'TNVASWWT'
028300030813     C                   WRITE     TIVLT000
028400030813     C*
028500030813     C                   ENDSR
028600030813
028700030417
028800030417
028900030417     C*----------------------------------------------------*
029000030417     C*  CONTROLLO NUMERICITA' CAMPI
029100030417     C*----------------------------------------------------*
029200030417     C     CHKNUM        BEGSR
029300030417     C*
029400030417     C                   call(e)   'ISNUMERIC'
029500030417     C                   PARM                    PiStr            30
029600030417     C                   PARM      ','           PiDecChr          1
029700030417     C                   PARM      *ZEROS        PiVal            30 9
029800030417     C                   PARM      '0'           PiInt             1
029900030417     C                   PARM      '0'           PiNum             1
030000030417     C                   IF        %error
030100030417     C                   EVAL      PiInt=*off
030200030417     C                   EVAL      PiNum=*off
030300030417     C                   ENDIF
030400030417     C*
030500030417     C                   ENDSR
030600030417     C***
030700030409
030800030410
030900030409
031000991027
031100991027      /TITLE Operazioni iniziali.
031200991027     C     *inzsr        BEGSR
031300991027     C*
031400991027     C     *ENTRY        PLIST
031500030409     C                   PARM                    prmppt
031600030409     C     wrkesito      PARM      wrkesito      prmesito
031700030813     C                   PARM                    prmksc
031800030813     C                   PARM                    prmksc2
031900030409     C*
032000030409     C* IMPOSTA LA DATA CORRENTE
032100030409     C                   TIME                    N14              14 0
032200030409     C                   MOVE      N14           N8                8 0
032300030813     C                   MOVEL     N14           ORACOR            6 0
032400030409     C                   Z-ADD     N8            G08DAT
032500030409     C                   Z-ADD     *ZEROS        G08INV
032600030409     C                   MOVEL     '0'           G08ERR
032700030409     C                   CALL      'XSRDA8'
032800030409     C                   PARM                    WLBDA8
032900030409     C                   Z-ADD     G08INV        DATCOR            8 0
033000030409     C****
033100030409     C* Definizione chiavi
033200030409     C****
033300030409     C* Chiave su TNCSB03L - completa
033400030409     C     KEYcsb03      KLIST
033500030429     C                   KFLD                    vasAAS
033600030429     C                   KFLD                    vasLNP
033700030429     C                   KFLD                    vasNRS
033800030429     C                   KFLD                    vasNSP
033900030409     C* Chiave su TITAS30C - parziale
034000030409     C     KEYtas30cP    KLIST
034100030429     C                   KFLD                    vasAAS
034200030429     C                   KFLD                    vasLNP
034300030429     C                   KFLD                    vasNRS
034400030429     C                   KFLD                    vasNSP
034500030409     C* Chiave su TIVIR01L - completa
034600030409     C     KEYvir01      KLIST
034700030409     C                   KFLD                    virKSC
034800030409     C                   KFLD                    virSEQ
034900991027     C*
035000991027     C                   ENDSR
035100030813** ctdata mc
035200030813ABCDEFGHILMN
