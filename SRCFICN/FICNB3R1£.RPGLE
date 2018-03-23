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
002500040727     d dataoggi        s               d   datfmt(*iso)
002600040727     d finoadata       s               d   datfmt(*iso) inz(d'2004-08-10')
002700950127     C*****************************************************************
002800020319     C     *ENTRY        PLIST
002900020320     C                   PARM                    ficnb3
003000040727     C*
003100040727     C                   TIME                    W0140            14 0
003200040727     C                   MOVEl     W0140         Wora              6 0
003300040727     C                   MOVE      W0140         WDAT              8 0
003400040727     C*
003500040727     C                   Z-ADD     Wdat          G02DAT
003600040727     C                   MOVEL     *BLANK        G02ERR
003700040727     C                   CALL      'XSRDA8'
003800040727     C                   PARM                    WLBDAT
003900040727     C* UDATE A 8 IN AAAA/MM/GG
004000040727     C                   move      G02INV        dataoggi
004100040727      *
004200020320      * per chiusura pgm
004300020320     c                   if        §T3tla ='C'
004400020320     c                   goto      fine
004500020320     c                   end
004600020319      *
004700020319     C     KTAB2         KLIST
004800020319     C                   KFLD                    CODUT
004900020319     C                   KFLD                    COD               2
005000020319      *
005100020319     C     KFGT          KLIST
005200020319     C                   KFLD                    §t3PDR
005300020319     C                   KFLD                    §t3SML
005400020319     C                   KFLD                    §t3TSR
005500091112      *
005600091112     C     Ktgt          KLIST
005700091112     C                   KFLD                    fgtPDR
005800091112     C                   KFLD                    fgtSML
005900091112     C                   KFLD                    fgtPRG
006000020408      *
006100020408     C     KFGT999       KLIST
006200020408     C                   KFLD                    §t3PDR
006300020408     C                   KFLD                    §t3SML
006400020408     C                   KFLD                    §t3TSR
006500020408     C                   KFLD                    tar999
006600020319      *
006700020319     C     KFTT          KLIST
006800020319     C                   KFLD                    §t3PDR
006900020319     C                   KFLD                    §t3FVL
007000020319     C                   KFLD                    §t3DDC
007100020319     C                   KFLD                    §t3TSR
007200020319      *
007300020319     C     KFTD          KLIST
007400020319     C                   KFLD                    fttPDR
007500020319     C                   KFLD                    fttTSR
007600020319     C                   KFLD                    fttNDC
007700020319     C                   KFLD                    fttDDC
007800020319     C*
007900020319     C                   Z-ADD     1             CODUT
008000020319     C                   CALL      'X§PARUT'
008100020319     C                   PARM                    UT§DSE
008200020319      *
008300020319     C* TAB.: TIPI PRESTAZIONE
008400020319     C                   Z-ADD     1             C                 5 0
008500020319     C                   MOVEL     '1Z'          COD
008600020319     C     KTAB2         SETLL     TABEL
008700020319     C     KTAB2         READE     TABEL                                  30
008800020319     C*
008900020319     C     *IN30         DOWEQ     *OFF
009000020319     C     TBLFLG        IFEQ      ' '
009100020319     C                   MOVEL     TBLUNI        DS1Z
009200020319     C     §1ZUPD        IFEQ      'S'
009300020319     C                   MOVEL     TBLKEY        TSR(C)
009400020319     C                   MOVEL     §1ZCTR        CTS(C)
009500020319     C                   ADD       1             C
009600020319     C                   ENDIF
009700020319     C                   ENDIF
009800020319     C     KTAB2         READE     TABEL                                  30
009900020319     C                   ENDDO
010000020319      *-----------------------------------------------------------------
010100020319     C                   MOVEL     *BLANKS       codtar            3
010200020319     C                   MOVEL     *BLANKS       flgctb            3
010300020403     C                   z-add     0             datcnv            8 0
010400020403     C                   z-add     0             datctb            8 0
010500020403     C                   z-add     0             datsta            8 0
010600020403     C                   z-add     0             datval            8 0
010700020507     C                   MOVEL     *BLANKS       §t3err
010800020319     C*
010900020408     c     §t3ctr        ifeq      *all'9'
011000020408     C* a prestazione/giornata
011100020408     C*                *-------------------*
011200020408     C                   EXSR      RIC_TARpresta
011300020408     C*                *-------------------*
011400020408     c                   else
011500020408     C* tariffa specifica
011600020408     C*                *-------------------*
011700020408     C                   EXSR      RIC_TAR
011800020408     C*                *-------------------*
011900020408     c                   end
012000020319      *
012100020319      *  imposta il codice tariffa se trovata
012200020319      *
012300020319     C                   MOVEL     codtar        §t3CTR
012400020403     C                   move      datcnv        §t3Cnv
012500020403     C                   move      datsta        §t3sta
012600020319      *
012700020320     c     fine          tag
012800020320     c                   if        §T3tla ='C' or §T3tla ='L'
012900950127     C                   SETON                                        LR
013000020320     c                   else
013100020320     c                   RETURN
013200020320     c                   end
013300020319     C*---------------------------------------------------------------*
013400020408     C* Tariffa specifica                                             *
013500020319     C*---------------------------------------------------------------*
013600020408     C     RIC_TARpresta BEGSR
013700020319     C*
013800020408     C*  poichè è già impostata a 999
013900020408     C                   MOVEL     *all'?'       codtar
014000020408     C                   move      §t3ctr        tar999            3 0
014100020408     C*
014200020408     C     KFGT999       CHAIN     FIFGT000                           68
014300020408     C*
014400020408    1C     *IN68         DOWEQ     *OFF
014500020408     C*
014600020408     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
014700020408     C*  e quelle diverse dalla divisa del documento
014800020408      ****   e quelle senza data di stampa (quindi non convalidate)
014900020408      ****   ma solo se non in simulazione.
015000020408      ****   se in simulazione non deve controllare la data di stampa.
015100020408    2C     fgtatb        IFEQ      ' '
015200020408     C     §t3div        andeq     fgtdiv
015300091112     C**** §t3DDC        ANDGE     fgtDDT
015400091112    4C**** §t3DDC        andLE     fgtDST
015500091112     C****
015600091112     C* Se tariffa del periodo
015700091112     C     Ktgt          CHAIN     FItgt01l
015800091112 2/3 c                   if        %Found(FItgt01l) and
015900091112     c                             §t3DDC >= tgtDDT and §t3DDC <= tgtDST
016000020507    3 *---
016100020507    3C     §t3sml        IFEQ      *blank
016200091112    3C     tgtDTS        andgt     0
016300091112    3C**** fgtDTS        andgt     0
016400020507    3C     §t3sml        oreq      'S'
016500020408    3 *---
016600020408     C                   MOVEL     FGTctr        codtar
016700091112     C*****              move      FGTdcn        datcnv
016800091112     C*****              move      FGTdts        datsta
016900091112     C                   move      tGTdcn        datcnv
017000091112     C                   move      tGTdts        datsta
017100020408     C*
017200020408    1C                   END
017300020507    3 *---
017400020507    3 * se presenti tariffe a prestazione/giornata senza data di stampa
017500020507    3C     §t3sml        IFEQ      *blank
017600091112    3C     tgtDTS        andeq     0
017700091112    3C*****fgtDTS        andeq     0
017800020507     C                   MOVEL     'E'           §t3err
017900020507    1C                   END
018000020507    3 *---
018100091112     c                   end
018200091112      *
018300020507    1C                   END
018400020408     C*
018500020408     C     KFGT999       READE     FIFGT01L                               68
018600020408    1C                   ENDDO
018700020408     C*
018800020408     C                   endSR
018900020408     C*---------------------------------------------------------------*
019000020408     C* Tariffa specifica                                             *
019100020408     C*---------------------------------------------------------------*
019200020408     c     RIC_TAR       BEGSR
019300020408     C*
019400020408     C* Controlla che tipi di bolle ci sono in distinta se Poste e/o Bartolini
019500020408     C*                *-------------------*
019600020408     C                   EXSR      RICBOL
019700020408     C*                *-------------------*
019800020408     C                   CLEAR                   btarv             3
019900020408     C                   CLEAR                   ptarv             3
020000020408     C                   CLEAR                   btardc            8 0
020100020408     C                   CLEAR                   ptardc            8 0
020200020408     C                   CLEAR                   btards            8 0
020300020408     C                   CLEAR                   ptards            8 0
020400020408     C*
020500020408     C     KFGT          CHAIN     FIFGT000                           68
020600020408     C*
020700020408    1C     *IN68         DOWEQ     *OFF
020800020408     C*
020900020408     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
021000020408     C*  e quelle diverse dalla divisa del documento
021100020408      ****   e quelle senza data di stampa (quindi non convalidate)
021200020408      ****   ma solo se non in simulazione.
021300020408      ****   se in simulazione non deve controllare la data di stampa.
021400020408    2C     fgtatb        IFEQ      ' '
021500020408     C     §t3div        andeq     fgtdiv
021600091112     C**** §t3DDC        ANDGE     fgtDDT
021700091112     C****
021800091112     C* Se tariffa del periodo
021900091112     C     Ktgt          CHAIN     FItgt01l
022000091112 2/3 c                   if        %Found(FItgt01l) and
022100091112     c                             §t3DDC >= tgtDDT and §t3DDC <= tgtDST
022200091112    3 *---
022300091112    3C     §t3sml        Ifeq      *blank
022400091112    3C     tgtDTS        andgt     0
022500091112    3C**** fgtDTS        andgt     0
022600091112    3C     §t3sml        oreq      'S'
022700020408    3 *---
022800020408      *
022900020408     C* ESCLUDO TARIFFA A GIORNATA E A PRESTAZ (COD.TAR = 999)
023000020408     C                   z-add     1             C
023100020408     C     fgtTSR        LOOKUP    TSR(C)                                 67
023200020408     C*
023300020408    3C     fgtCTR        IFNE      CTS(C)
023400091112    4C**** §t3DDC        andLE     FGTDST
023500020408     C*
023600020408     C* EMETTO LA PREFERENZIALE SE C'E'
023700020408    5C     fgtFTP        ifeq      'P'
023800020408     C                   MOVEL     FGTCTR        btarv
023900091112     C*****              z-add     FGTdcn        btardc
024000091112     C*****              z-add     FGTdts        btards
024100091112     C                   z-add     tGTdcn        btardc
024200091112     C                   z-add     tGTdts        btards
024300020408     C                   leave
024400020408   X5C                   end
024500020408     C*
024600020408     C* SE NON E' PREFERENZIALE TENGO MEMORIZZATA LA PRIMA
024700020408    6C     btarv         IFEQ      *BLANKS
024800020408     C     fgtFTP        ANDNE     'O'
024900040628    6C     btarv         Orne      *BLANKS
025000040628    6C     fgtCTR        andeq     sav_FTDCTB
025100040628     C     fgtFTP        ANDNE     'O'
025200020408     C                   MOVEL     fgtCTR        btarv
025300091112     C****               z-add     FGTdcn        btardc
025400091112     C****               z-add     FGTdts        btards
025500091112     C                   z-add     tGTdcn        btardc
025600091112     C                   z-add     tGTdts        btards
025700020408    6C                   ENDIF
025800020408      *
025900020408    6C     ptarv         IFEQ      *BLANKS
026000020408     C     FGTFTP        ANDEQ     'O'
026100040628    6C     ptarv         Orne      *BLANKS
026200040628    6C     fgtCTR        andeq     sav_FTDCTP
026300040628     C     FGTFTP        ANDEQ     'O'
026400020408     C                   MOVEL     FGTCTR        ptarv
026500091112     C*****              z-add     FGTdcn        ptardc
026600091112     C*****              z-add     FGTdts        ptards
026700091112     C                   z-add     tGTdcn        ptardc
026800091112     C                   z-add     tGTdts        ptards
026900020408    6C                   ENDIF
027000020408      *
027100020408    3C                   ENDIF
027200020408      *
027300091112     c                   end
027400091112     c                   end
027500091112      *
027600020408    2C                   ENDIF
027700020408     C*
027800020408     C     KFGT          READE     FIFGT000                               68
027900020408    1C                   ENDDO
028000020408     C*
028100020408     C* SE CI SONO BOLLE BARTOLINI MUOVO LA TARIFFA BARTOLINI
028200020408    0C     flgbar        IFEQ      'S'
028300020408    1C     btarv         andne     *BLANKS
028400040727     ** oppure deve prendere la tariffa Bartolini se non presente la tar.poste dal 10/08/2004
028500040727     c     flgbar        oreq      *blank
028600040727     c     flgpos        andeq     'S'
028700040727    1C     ptarv         andeq     *blank
028800040727    1C     dataoggi      andge     finoadata
028900040727     **
029000020408     C                   MOVEL     btarv         codtar                          VALIDA
029100020408     C                   z-add     btardc        datcnv
029200020408     C                   z-add     btards        datsta
029300020408     C* SALVO LA TARIFFA BARTOLINI PER IL DETTAGLIO SPEDIZIONE
029400020408     C                   MOVEL     codtar        flgctb
029500020408     C                   z-add     datcnv        datctb
029600020408     C                   z-add     datsta        datval
029700020408    0C                   ENDIF
029800020408     C*
029900020408     C* SE CI SONO BOLLE POSTE MUOVO LA TARIFFA POSTA
030000020408    0C     flgpos        IFEQ      'S'
030100020408    1C     ptarv         andne     *BLANKS
030200020408     C                   MOVEL     ptarv         codtar                          VALIDA
030300020408     C                   z-add     ptardc        datcnv
030400020408     C                   z-add     ptards        datsta
030500020408    0C                   ENDIF
030600020408     C*
030700020408     C* SE NON NE HO TRAVATA NESSUNA --> EMETTO ???
030800020408     C     codtar        IFEQ      *BLANKS
030900020408     C                   MOVEL     '???'         codtar
031000020408     C                   z-add     0             datcnv
031100020408     C                   z-add     0             datsta
031200020408     C                   ELSE
031300020408     C*
031400020408     C* SE HO UNA TARIFFA BARTOLINI  PROPONGO SEMPRE QUELLA
031500020408     C     flgctb        IFNE      *BLANKS
031600020408     C                   MOVEL     flgctb        codtar
031700020408     C                   z-add     datctb        datcnv
031800020408     C                   z-add     datval        datsta
031900020408     C                   ENDIF
032000020408     C*
032100020408     C                   ENDIF
032200020408     C*
032300020408     C                   endSR
032400020408     C*---------------------------------------------------------------*
032500020408     C* RICERCA TIPOLOGIA BOLLE PER RICERCA TARIFFA ------------------*
032600020408     C*---------------------------------------------------------------*
032700020408     C     RICBOL        BEGSR
032800020408     C*
032900020319     C*  flags Bolle Poste e Bartolini
033000020319     C                   MOVEL     ' '           flgpos            1
033100020319     C                   MOVEL     ' '           flgbar            1
033200030108     c                   z-add     0             conta             5 0
033300040628     c                   z-add     0             sav_FTDCTP        3 0
033400040628     c                   z-add     0             sav_FTDCTB        3 0
033500020319     C*
033600020319     C* LETTURA SEQUENZIALE DEL DETTAGLIO DISTINTA PADRONCINI PER RECUPERARE LA LNP
033700020319     C* UTILE PER RICONOSCERE SE BOLLA POSTA
033800020319     C     kftt          chain     FIFTT02L                           66
033900020319     C**
034000020319     C     *IN66         DOWEQ     *OFF
034100020319     C**
034200020319     C     KFTD          chain     fiftd01l                           67
034300020319     C     *in67         DOweq     *Off
034400030108     c                   add       1             conta
034500020319     C**
034600020319     C* CONTROLLO IN AZORG SE PO POSTE
034700020319     C                   clear                   OG143
034800020319     C     FTDLNP        CHAIN     AZORG01L                           66
034900020319     C  N66              MOVEL     ORGDE3        OG143
035000020319     C*
035100020319     C*  P.O. poste
035200020717     C     §OgNTW        IFEQ      'PPT'
035300040628     c                   if        ftdctp <> *blank and sav_FTDCTP=0
035400040628     c                   move      FTDCTP        sav_FTDCTP        3 0
035500040628     c                   end
035600020319     C                   MOVEL     'S'           flgpos
035700020319     C                   ELSE
035800020319     C*  bartolini
035900040628     c                   if        ftdctp <> *blank and sav_FTDCTB=0
036000040628     c                   move      FTDCTP        sav_FTDCTB        3 0
036100040628     c                   end
036200020319     C                   MOVEL     'S'           flgbar
036300020319     C                   ENDIF
036400020319     C*
036500020319     C     KFTD          reade     fiftd01l                               67
036600020319     C                   ENDDO
036700020319     C**
036800020319     C     KFTT          READE     FIFTT02L                               66
036900020319     C                   ENDDO
037000040727     **
037100040727     ** Memorizza se sono solo spedizioni poste la tariffa sul sav Bartolini
037200040727     C                   if        flgBAR = ' ' and flgPOS = 'S'
037300040727     c                   move      sav_FTDctp    sav_FTDctb
037400040727     c                   end
037500040727     **
037600030108     C*
037700030108     C* Attenzione: Occorre gestire una eccezione.
037800030108     C*  Se ci troviamo in presenza di una testata Ritiri senza nessuna riga
037900030108     C*  di dettaglio e con il campo Nr.RITIRI ANNULLATI >0 quindi stiamo
038000030108     C*  prendendo in considerazione solo dei Ritiri Annullati devo impostare
038100030108     C*  il FLGBAR come se avessi trovato solo delle bolle Bartolini.
038200030108     C* Questo accade quando sono stati preventivati enne ritiri e tutti gli
038300030108     C* enne ritiri sono stati annullati. In tal modo non si ha più la capacità
038400030108     C* di sapere se erano Bartolini o Poste quindi si imposta la tariffa
038500030108     C* su Bartolini. (Per Default)
038600030108     c                   if        §t3TSR='R' and conta=0 and §t3RAN = 'S'
038700030108     c                             and flgBAR = ' ' and flgPOS = ' '
038800030108     C                   MOVEL     'S'           flgBAR
038900030108     C                   EndIF
039000030108     C*
039100020319     C*
039200020319     C                   ENDSR
039300020319     C*---------------------------------------------------------------*
