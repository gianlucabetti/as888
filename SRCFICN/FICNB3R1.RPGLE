000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200020319     H* FICNB3R1 *----------------------------------------------------*
000300020319     H*         - cerca Tariffa x conteggi
000400950127     H*--------------------------------------------------------------*
000500020319     FTABEL00F  IF   E           K DISK
000600000711     FAZORG01L  IF   E           K DISK
000700020319     FFIFGT01L  IF   E           K DISK
000800091112     FFItgt01L  IF   E           K DISK
000900020319     FFIFTT02L  IF   E           K DISK
001000011113     FFIFTD01L  IF   E           K DISK
001100020319     F*--------------------------------------------------------------*
001200020319     D TSR             S              1    DIM(90)                              TIPI PRESTAZ
001300020319     D CTS             S              3  0 DIM(90)                              CDTAR TIP PRESTAZ
001400020319     D FICNB3        E DS                  EXTNAME(FICNB3DS)
001500000711     D OG143         E DS
001600020319     D DS1Z          E DS
001700020319     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800040727     **
001900040727     D WLBDAT          DS
002000040727     D  G02DAT                 1      8  0
002100040727     D  G02INV                 9     16  0
002200040727     D  G02ERR                17     17
002300040727     D  G02TGI                18     22  0
002400040727     **
002500130320     d  codSOC                             like(§T3SOC)
002600130320     d  codCDF                             like(§T3CDF)
002700130320     d  codFLR                             like(§T3FLR)
002800130320     d  datDRC                             like(§T3DRC)
002900130320     d  datDDT                             like(§T3DDT)
003000130320     d  datDST                             like(§T3DST)
003100130321     d  codPRG                             like(§T3PRG)
003200130320     **
003300130320     d BcodSOC                             like(§T3SOC)
003400130320     d BcodCDF                             like(§T3CDF)
003500130320     d BcodFLR                             like(§T3FLR)
003600130320     d BdatDRC                             like(§T3DRC)
003700130320     d BdatDDT                             like(§T3DDT)
003800130320     d BdatDST                             like(§T3DST)
003900130321     d BcodPRG                             like(§T3PRG)
004000130321     **
004100130321     d BcodSOCsv                           like(§T3SOC)
004200130321     d BcodCDFsv                           like(§T3CDF)
004300130321     d BcodFLRsv                           like(§T3FLR)
004400130321     d BdatDRCsv                           like(§T3DRC)
004500130321     d BdatDDTsv                           like(§T3DDT)
004600130321     d BdatDSTsv                           like(§T3DST)
004700130321     d BcodPRGsv                           like(§T3PRG)
004800130321     **
004900130320     d PcodSOC                             like(§T3SOC)
005000130320     d PcodCDF                             like(§T3CDF)
005100130320     d PcodFLR                             like(§T3FLR)
005200130320     d PdatDRC                             like(§T3DRC)
005300130320     d PdatDDT                             like(§T3DDT)
005400130320     d PdatDST                             like(§T3DST)
005500130321     d PcodPRG                             like(§T3PRG)
005600130320     **
005700040727     d dataoggi        s               d   datfmt(*iso)
005800040727     d finoadata       s               d   datfmt(*iso) inz(d'2004-08-10')
005900950127     C*****************************************************************
006000020319     C     *ENTRY        PLIST
006100020320     C                   PARM                    ficnb3
006200040727     C*
006300040727     C                   TIME                    W0140            14 0
006400040727     C                   MOVEl     W0140         Wora              6 0
006500040727     C                   MOVE      W0140         WDAT              8 0
006600040727     C*
006700040727     C                   Z-ADD     Wdat          G02DAT
006800040727     C                   MOVEL     *BLANK        G02ERR
006900040727     C                   CALL      'XSRDA8'
007000040727     C                   PARM                    WLBDAT
007100040727     C* UDATE A 8 IN AAAA/MM/GG
007200040727     C                   move      G02INV        dataoggi
007300040727      *
007400020320      * per chiusura pgm
007500020320     c                   if        §T3tla ='C'
007600020320     c                   goto      fine
007700020320     c                   end
007800020319      *
007900020319     C     KTAB2         KLIST
008000020319     C                   KFLD                    CODUT
008100020319     C                   KFLD                    COD               2
008200020319      *
008300020319     C     KFGT          KLIST
008400020319     C                   KFLD                    §t3PDR
008500020319     C                   KFLD                    §t3SML
008600020319     C                   KFLD                    §t3TSR
008700091112      *
008800091112     C     Ktgt          KLIST
008900091112     C                   KFLD                    fgtPDR
009000091112     C                   KFLD                    fgtSML
009100091112     C                   KFLD                    fgtPRG
009200020408      *
009300020408     C     KFGT999       KLIST
009400020408     C                   KFLD                    §t3PDR
009500020408     C                   KFLD                    §t3SML
009600020408     C                   KFLD                    §t3TSR
009700020408     C                   KFLD                    tar999
009800020319      *
009900020319     C     KFTT          KLIST
010000020319     C                   KFLD                    §t3PDR
010100020319     C                   KFLD                    §t3FVL
010200020319     C                   KFLD                    §t3DDC
010300020319     C                   KFLD                    §t3TSR
010400020319      *
010500020319     C     KFTD          KLIST
010600020319     C                   KFLD                    fttPDR
010700020319     C                   KFLD                    fttTSR
010800020319     C                   KFLD                    fttNDC
010900020319     C                   KFLD                    fttDDC
011000020319     C*
011100020319     C                   Z-ADD     1             CODUT
011200020319     C                   CALL      'X§PARUT'
011300020319     C                   PARM                    UT§DSE
011400020319      *
011500020319     C* TAB.: TIPI PRESTAZIONE
011600020319     C                   Z-ADD     1             C                 5 0
011700020319     C                   MOVEL     '1Z'          COD
011800020319     C     KTAB2         SETLL     TABEL
011900020319     C     KTAB2         READE     TABEL                                  30
012000020319     C*
012100020319     C     *IN30         DOWEQ     *OFF
012200020319     C     TBLFLG        IFEQ      ' '
012300020319     C                   MOVEL     TBLUNI        DS1Z
012400020319     C     §1ZUPD        IFEQ      'S'
012500020319     C                   MOVEL     TBLKEY        TSR(C)
012600020319     C                   MOVEL     §1ZCTR        CTS(C)
012700020319     C                   ADD       1             C
012800020319     C                   ENDIF
012900020319     C                   ENDIF
013000020319     C     KTAB2         READE     TABEL                                  30
013100020319     C                   ENDDO
013200130320      *
013300020319      *-----------------------------------------------------------------
013400130320      *
013500020319     C                   MOVEL     *BLANKS       codtar            3
013600020319     C                   MOVEL     *BLANKS       flgctb            3
013700020403     C                   z-add     0             datcnv            8 0
013800020403     C                   z-add     0             datctb            8 0
013900020403     C                   z-add     0             datsta            8 0
014000020403     C                   z-add     0             datval            8 0
014100130320     C                   z-add     0             datDRC
014200130320     C                   z-add     0             datDDT
014300130320     C                   z-add     0             datDST
014400130321     C                   z-add     0             codPRG
014500130320     c                   clear                   codSOC
014600130320     c                   clear                   codCDF
014700130320     c                   clear                   codFLR
014800130321     C                   clear                   BdatDRCsv
014900130321     C                   clear                   BdatDDTsv
015000130321     C                   clear                   BdatDSTsv
015100130321     C                   clear                   BcodSOCsv
015200130321     C                   clear                   BcodCDFsv
015300130321     C                   clear                   BcodFLRsv
015400130321     C                   clear                   BcodFLRsv
015500130321     C                   clear                   BcodPRGsv
015600130320      *
015700020507     C                   MOVEL     *BLANKS       §t3err
015800130320     c                   clear                   §T3DRC
015900130320     c                   clear                   §T3DDT
016000130320     c                   clear                   §T3DST
016100130320     c                   clear                   §T3SOC
016200130320     c                   clear                   §T3CDF
016300130320     c                   clear                   §T3FLR
016400130321     c                   clear                   §T3PRG
016500020319     C*
016600020408     c     §t3ctr        ifeq      *all'9'
016700020408     C* a prestazione/giornata
016800020408     C*                *-------------------*
016900020408     C                   EXSR      RIC_TARpresta
017000020408     C*                *-------------------*
017100020408     c                   else
017200020408     C* tariffa specifica
017300020408     C*                *-------------------*
017400020408     C                   EXSR      RIC_TAR
017500020408     C*                *-------------------*
017600020408     c                   end
017700020319      *
017800020319      *  imposta il codice tariffa se trovata
017900020319      *
018000020319     C                   MOVEL     codtar        §t3CTR
018100020403     C                   move      datcnv        §t3Cnv
018200020403     C                   move      datsta        §t3sta
018300130320      *
018400130320     C                   move      datDRC        §T3DRC
018500130320     C                   move      datDDT        §T3DDT
018600130320     C                   move      datDST        §T3DST
018700130320     C                   move      codSOC        §T3SOC
018800130320     C                   move      codCDF        §T3CDF
018900130320     C                   move      codFLR        §T3FLR
019000130321     C                   move      codPRG        §T3PRG
019100020319      *
019200020320     c     fine          tag
019300020320     c                   if        §T3tla ='C' or §T3tla ='L'
019400950127     C                   SETON                                        LR
019500020320     c                   else
019600020320     c                   RETURN
019700020320     c                   end
019800020319     C*---------------------------------------------------------------*
019900020408     C* Tariffa specifica                                             *
020000020319     C*---------------------------------------------------------------*
020100020408     C     RIC_TARpresta BEGSR
020200020319     C*
020300020408     C*  poichè è già impostata a 999
020400020408     C                   MOVEL     *all'?'       codtar
020500020408     C                   move      §t3ctr        tar999            3 0
020600020408     C*
020700020408     C     KFGT999       CHAIN     FIFGT000                           68
020800020408     C*
020900020408    1C     *IN68         DOWEQ     *OFF
021000020408     C*
021100020408     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
021200020408     C*  e quelle diverse dalla divisa del documento
021300020408      ****   e quelle senza data di stampa (quindi non convalidate)
021400020408      ****   ma solo se non in simulazione.
021500020408      ****   se in simulazione non deve controllare la data di stampa.
021600020408    2C     fgtatb        IFEQ      ' '
021700020408     C     §t3div        andeq     fgtdiv
021800091112     C****
021900091112     C* Se tariffa del periodo
022000091112     C     Ktgt          CHAIN     FItgt01l
022100091112 2/3 c                   if        %Found(FItgt01l) and
022200091112     c                             §t3DDC >= tgtDDT and §t3DDC <= tgtDST
022300020507    3 *---
022400020507    3C     §t3sml        IFEQ      *blank
022500091112    3C     tgtDTS        andgt     0
022600020507    3C     §t3sml        oreq      'S'
022700020408    3 *---
022800020408     C                   MOVEL     FGTctr        codtar
022900091112     C                   move      tGTdcn        datcnv
023000091112     C                   move      tGTdts        datsta
023100130320      *
023200130320     C                   move      tGTDRC        datDRC
023300130320     C                   move      tGTDDT        datDDT
023400130320     C                   move      tGTDST        datDST
023500130320     C                   move      tGTSOC        codSOC
023600130320     C                   move      tGTCDF        codCDF
023700130320     C                   move      tGTFLR        codFLR
023800130321     C                   move      tGTPRG        codPRG
023900020408    1C                   END
024000020507    3 *---
024100020507    3 * se presenti tariffe a prestazione/giornata senza data di stampa
024200020507    3C     §t3sml        IFEQ      *blank
024300091112    3C     tgtDTS        andeq     0
024400020507     C                   MOVEL     'E'           §t3err
024500020507    1C                   END
024600020507    3 *---
024700091112     c                   end
024800091112      *
024900020507    1C                   END
025000020408     C*
025100020408     C     KFGT999       READE     FIFGT01L                               68
025200020408    1C                   ENDDO
025300020408     C*
025400020408     C                   endSR
025500020408     C*---------------------------------------------------------------*
025600020408     C* Tariffa specifica                                             *
025700020408     C*---------------------------------------------------------------*
025800020408     c     RIC_TAR       BEGSR
025900020408     C*
026000020408     C* Controlla che tipi di bolle ci sono in distinta se Poste e/o Bartolini
026100020408     C*                *-------------------*
026200020408     C                   EXSR      RICBOL
026300020408     C*                *-------------------*
026400020408     C                   CLEAR                   btarv             3
026500020408     C                   CLEAR                   ptarv             3
026600020408     C                   CLEAR                   btardc            8 0
026700020408     C                   CLEAR                   ptardc            8 0
026800020408     C                   CLEAR                   btards            8 0
026900020408     C                   CLEAR                   ptards            8 0
027000130320     C                   CLEAR                   BcodSOC
027100130320     C                   CLEAR                   BcodCDF
027200130320     C                   CLEAR                   BcodFLR
027300130320     C                   CLEAR                   BdatDRC
027400130320     C                   CLEAR                   BdatDDT
027500130320     C                   CLEAR                   BdatDST
027600130321     C                   CLEAR                   BcodPRG
027700130320     C                   CLEAR                   PcodSOC
027800130320     C                   CLEAR                   PcodCDF
027900130320     C                   CLEAR                   PcodFLR
028000130320     C                   CLEAR                   PdatDRC
028100130320     C                   CLEAR                   PdatDDT
028200130320     C                   CLEAR                   PdatDST
028300130321     C                   CLEAR                   PcodPRG
028400020408     C*
028500020408     C     KFGT          CHAIN     FIFGT000                           68
028600020408     C*
028700020408    1C     *IN68         DOWEQ     *OFF
028800020408     C*
028900020408     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
029000020408     C*  e quelle diverse dalla divisa del documento
029100020408      ****   e quelle senza data di stampa (quindi non convalidate)
029200020408      ****   ma solo se non in simulazione.
029300020408      ****   se in simulazione non deve controllare la data di stampa.
029400020408    2C     fgtatb        IFEQ      ' '
029500020408     C     §t3div        andeq     fgtdiv
029600091112     C****
029700091112     C* Se tariffa del periodo
029800091112     C     Ktgt          CHAIN     FItgt01l
029900091112 2/3 c                   if        %Found(FItgt01l) and
030000091112     c                             §t3DDC >= tgtDDT and §t3DDC <= tgtDST
030100091112    3 *---
030200091112    3C     §t3sml        Ifeq      *blank
030300091112    3C     tgtDTS        andgt     0
030400091112    3C     §t3sml        oreq      'S'
030500020408    3 *---
030600020408      *
030700020408     C* ESCLUDO TARIFFA A GIORNATA E A PRESTAZ (COD.TAR = 999)
030800020408     C                   z-add     1             C
030900020408     C     fgtTSR        LOOKUP    TSR(C)                                 67
031000020408     C*
031100020408    3C     fgtCTR        IFNE      CTS(C)
031200020408     C*
031300020408     C* EMETTO LA PREFERENZIALE SE C'E'
031400020408    5C     fgtFTP        ifeq      'P'
031500020408     C                   MOVEL     FGTCTR        btarv
031600091112     C                   z-add     tGTdcn        btardc
031700091112     C                   z-add     tGTdts        btards
031800130320     c                   movel     tGTSOC        BcodSOC
031900130320     c                   movel     tGTCDF        BcodCDF
032000130320     c                   movel     tGTFLR        BcodFLR
032100130320     c                   z-add     tGTDRC        BdatDRC
032200130320     c                   z-add     tGTDDT        BdatDDT
032300130320     c                   z-add     tGTDST        BdatDST
032400130321     c                   z-add     tGTPRG        BcodPRG
032500020408     C                   leave
032600020408   X5C                   end
032700020408     C*
032800020408     C* SE NON E' PREFERENZIALE TENGO MEMORIZZATA LA PRIMA
032900020408    6C     btarv         IFEQ      *BLANKS
033000020408     C     fgtFTP        ANDNE     'O'
033100040628    6C     btarv         Orne      *BLANKS
033200040628    6C     fgtCTR        andeq     sav_FTDCTB
033300040628     C     fgtFTP        ANDNE     'O'
033400020408     C                   MOVEL     fgtCTR        btarv
033500091112     C                   z-add     tGTdcn        btardc
033600091112     C                   z-add     tGTdts        btards
033700130320     c                   movel     tGTSOC        BcodSOC
033800130320     c                   movel     tGTCDF        BcodCDF
033900130320     c                   movel     tGTFLR        BcodFLR
034000130320     c                   z-add     tGTDRC        BdatDRC
034100130320     c                   z-add     tGTDDT        BdatDDT
034200130320     c                   z-add     tGTDST        BdatDST
034300130321     c                   z-add     tGTPRG        BcodPRG
034400020408    6C                   ENDIF
034500020408      *
034600020408    6C     ptarv         IFEQ      *BLANKS
034700020408     C     FGTFTP        ANDEQ     'O'
034800040628    6C     ptarv         Orne      *BLANKS
034900040628    6C     fgtCTR        andeq     sav_FTDCTP
035000040628     C     FGTFTP        ANDEQ     'O'
035100020408     C                   MOVEL     FGTCTR        ptarv
035200091112     C                   z-add     tGTdcn        ptardc
035300091112     C                   z-add     tGTdts        ptards
035400130320     c                   movel     tGTSOC        PcodSOC
035500130320     c                   movel     tGTCDF        PcodCDF
035600130320     c                   movel     tGTFLR        PcodFLR
035700130320     c                   z-add     tGTDRC        PdatDRC
035800130320     c                   z-add     tGTDDT        PdatDDT
035900130320     c                   z-add     tGTDST        PdatDST
036000130321     c                   z-add     tGTPRG        PcodPRG
036100020408    6C                   ENDIF
036200020408      *
036300020408    3C                   ENDIF
036400020408      *
036500091112     c                   end
036600091112     c                   end
036700091112      *
036800020408    2C                   ENDIF
036900020408     C*
037000020408     C     KFGT          READE     FIFGT000                               68
037100020408    1C                   ENDDO
037200020408     C*
037300020408     C* SE CI SONO BOLLE BARTOLINI MUOVO LA TARIFFA BARTOLINI
037400020408    0C     flgbar        IFEQ      'S'
037500020408    1C     btarv         andne     *BLANKS
037600130321      *
037700040727     ** oppure deve prendere la tariffa Bartolini se non presente la tar.poste dal 10/08/2004
037800040727     c     flgbar        oreq      *blank
037900040727     c     flgpos        andeq     'S'
038000040727    1C     ptarv         andeq     *blank
038100040727    1C     dataoggi      andge     finoadata
038200040727     **
038300020408     C                   MOVEL     btarv         codtar                          VALIDA
038400020408     C                   z-add     btardc        datcnv
038500020408     C                   z-add     btards        datsta
038600130320      *
038700020408     C* SALVO LA TARIFFA BARTOLINI PER IL DETTAGLIO SPEDIZIONE
038800020408     C                   MOVEL     codtar        flgctb
038900020408     C                   z-add     datcnv        datctb
039000020408     C                   z-add     datsta        datval
039100130321     C                   z-add     BdatDRC       BdatDRCsv
039200130321     C                   z-add     BdatDDT       BdatDDTsv
039300130321     C                   z-add     BdatDST       BdatDSTsv
039400130321     C                   movel     BcodSOC       BcodSOCsv
039500130321     C                   movel     BcodCDF       BcodCDFsv
039600130321     C                   movel     BcodFLR       BcodFLRsv
039700130321     C                   z-add     BcodPRG       BcodPRGsv
039800130321      *-
039900130321     C                   z-add     BdatDRC       datDRC
040000130321     C                   z-add     BdatDDT       datDDT
040100130321     C                   z-add     BdatDST       datDST
040200130321     C                   movel     BcodSOC       codSOC
040300130321     C                   movel     BcodCDF       codCDF
040400130321     C                   movel     BcodFLR       codFLR
040500130321     C                   z-add     BcodPRG       codPRG
040600130321      *
040700020408    0C                   ENDIF
040800020408     C*
040900020408     C* SE CI SONO BOLLE POSTE MUOVO LA TARIFFA POSTA
041000020408    0C     flgpos        IFEQ      'S'
041100020408    1C     ptarv         andne     *BLANKS
041200020408     C                   MOVEL     ptarv         codtar                          VALIDA
041300020408     C                   z-add     ptardc        datcnv
041400020408     C                   z-add     ptards        datsta
041500130320      *-
041600130320     C                   z-add     PdatDRC       datDRC
041700130320     C                   z-add     PdatDDT       datDDT
041800130320     C                   z-add     PdatDST       datDST
041900130320     C                   movel     PcodSOC       codSOC
042000130320     C                   movel     PcodCDF       codCDF
042100130320     C                   movel     PcodFLR       codFLR
042200130321     C                   z-add     PcodPRG       codPRG
042300020408    0C                   ENDIF
042400020408     C*
042500020408     C* SE NON NE HO TRAVATA NESSUNA --> EMETTO ???
042600020408     C     codtar        IFEQ      *BLANKS
042700020408     C                   MOVEL     '???'         codtar
042800020408     C                   z-add     0             datcnv
042900020408     C                   z-add     0             datsta
043000130320      *
043100130320     C                   clear                   datDRC
043200130320     C                   clear                   datDDT
043300130320     C                   clear                   datDST
043400130320     C                   clear                   codSOC
043500130320     C                   clear                   codCDF
043600130320     C                   clear                   codFLR
043700130321     C                   clear                   codPRG
043800130320      *
043900020408     C                   ELSE
044000020408     C*
044100020408     C* SE HO UNA TARIFFA BARTOLINI  PROPONGO SEMPRE QUELLA
044200020408     C     flgctb        IFNE      *BLANKS
044300020408     C                   MOVEL     flgctb        codtar
044400020408     C                   z-add     datctb        datcnv
044500020408     C                   z-add     datval        datsta
044600130320      *
044700130321     C                   z-add     BdatDRCsv     datDRC
044800130321     C                   z-add     BdatDDTsv     datDDT
044900130321     C                   z-add     BdatDSTsv     datDST
045000130321     C                   movel     BcodSOCsv     codSOC
045100130321     C                   movel     BcodCDFsv     codCDF
045200130321     C                   movel     BcodFLRsv     codFLR
045300130321     C                   z-add     BcodPRGsv     codPRG
045400130320      *
045500020408     C                   ENDIF
045600020408     C*
045700020408     C                   ENDIF
045800020408     C*
045900020408     C                   endSR
046000020408     C*---------------------------------------------------------------*
046100020408     C* RICERCA TIPOLOGIA BOLLE PER RICERCA TARIFFA ------------------*
046200020408     C*---------------------------------------------------------------*
046300020408     C     RICBOL        BEGSR
046400020408     C*
046500020319     C*  flags Bolle Poste e Bartolini
046600020319     C                   MOVEL     ' '           flgpos            1
046700020319     C                   MOVEL     ' '           flgbar            1
046800030108     c                   z-add     0             conta             5 0
046900040628     c                   z-add     0             sav_FTDCTP        3 0
047000040628     c                   z-add     0             sav_FTDCTB        3 0
047100020319     C*
047200020319     C* LETTURA SEQUENZIALE DEL DETTAGLIO DISTINTA PADRONCINI PER RECUPERARE LA LNP
047300020319     C* UTILE PER RICONOSCERE SE BOLLA POSTA
047400020319     C     kftt          chain     FIFTT02L                           66
047500020319     C**
047600020319     C     *IN66         DOWEQ     *OFF
047700020319     C**
047800020319     C     KFTD          chain     fiftd01l                           67
047900020319     C     *in67         DOweq     *Off
048000030108     c                   add       1             conta
048100020319     C**
048200020319     C* CONTROLLO IN AZORG SE PO POSTE
048300020319     C                   clear                   OG143
048400020319     C     FTDLNP        CHAIN     AZORG01L                           66
048500020319     C  N66              MOVEL     ORGDE3        OG143
048600020319     C*
048700020319     C*  P.O. poste
048800020717     C     §OgNTW        IFEQ      'PPT'
048900040628     c                   if        ftdctp <> *blank and sav_FTDCTP=0
049000040628     c                   move      FTDCTP        sav_FTDCTP        3 0
049100040628     c                   end
049200020319     C                   MOVEL     'S'           flgpos
049300020319     C                   ELSE
049400020319     C*  bartolini
049500040628     c                   if        ftdctp <> *blank and sav_FTDCTB=0
049600040628     c                   move      FTDCTP        sav_FTDCTB        3 0
049700040628     c                   end
049800020319     C                   MOVEL     'S'           flgbar
049900020319     C                   ENDIF
050000020319     C*
050100020319     C     KFTD          reade     fiftd01l                               67
050200020319     C                   ENDDO
050300020319     C**
050400020319     C     KFTT          READE     FIFTT02L                               66
050500020319     C                   ENDDO
050600040727     **
050700040727     ** Memorizza se sono solo spedizioni poste la tariffa sul sav Bartolini
050800040727     C                   if        flgBAR = ' ' and flgPOS = 'S'
050900040727     c                   move      sav_FTDctp    sav_FTDctb
051000040727     c                   end
051100030108     C*
051200030108     C* Attenzione: Occorre gestire una eccezione.
051300030108     C*  Se ci troviamo in presenza di una testata Ritiri senza nessuna riga
051400030108     C*  di dettaglio e con il campo Nr.RITIRI ANNULLATI >0 quindi stiamo
051500030108     C*  prendendo in considerazione solo dei Ritiri Annullati devo impostare
051600030108     C*  il FLGBAR come se avessi trovato solo delle bolle Bartolini.
051700030108     C* Questo accade quando sono stati preventivati enne ritiri e tutti gli
051800030108     C* enne ritiri sono stati annullati. In tal modo non si ha più la capacità
051900030108     C* di sapere se erano Bartolini o Poste quindi si imposta la tariffa
052000030108     C* su Bartolini. (Per Default)
052100030108     c                   if        §t3TSR='R' and conta=0 and §t3RAN = 'S'
052200030108     c                             and flgBAR = ' ' and flgPOS = ' '
052300030108     C                   MOVEL     'S'           flgBAR
052400030108     C                   EndIF
052500030108     C*
052600020319     C                   ENDSR
052700020319     C*---------------------------------------------------------------*
