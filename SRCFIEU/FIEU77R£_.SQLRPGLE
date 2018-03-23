000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400080416     H DECEDIT('0,') DATEDIT(*YMD/)
000500041103      ***********************************************************************
000600040909     Ffieu77D   CF   E             WORKSTN
000700080416     Ftnecx00f  uf a E           k disk    prefix(ECX_)
000800040909      *****************************************************************
000900030620      *
001000030624     D KPJBA         E DS
001100030625     D OG143         E DS
001200040909     D*
001300040909     D WLBDAT          DS
001400040909     D  G02DAT                 1      8  0
001500040909     D  G02INV                 9     16  0
001600040909     D  G02ERR                17     17
001700040909     D  G02TGI                18     22  0
001800040909      *
001900030625     D parDate         DS
002000030625     D   DaData                1      8  0
002100030625     D    aData                9     16  0
002200030625     D    aAnno                9     12  0
002300030625     D    aMesG               13     16  0
002400030703     D   TpNtw                17     19
002500030703     D   TpData               20     20
002600030625      *
002700030625     D parBolla        DS
002800030625     D   tasAAS                1      4  0
002900030625     D   tasLnp                5      7  0
003000030625     D   tasNrs                8      9  0
003100030625     D   tasNsp               10     16  0
003200030625     D   tasTbl               17     18
003300030703     D   KNtw                 19     21
003400041013      *
003500041013     D Linee           S              3    DIM(20) INZ(' ')
003600080416     D UltimaData      s              8  0
003700080416     D UltimaData_X    s              8  0
003800080416     D data_Iso        s               d   datfmt(*ISO)
003900080416     D d1NTWsav        s                   like(d1Ntw)
004000030620      *---------------------------------------------------------------------*
004100030624     C     *entry        plist
004200030624     C                   parm                    kpjba
004300040909      **
004400041015      ** prima di attivare il programma controlla allineamento della
004500041015      **  tabella TNEVA con la tabella delle VARIE "CC"
004600041015      **    Se disallineata avvisa ed esce dal programma senza importare
004700041015      **     i dati.
004800041015     c                   exsr      CTR_tab_EVA
004900041015     c                   if        tab_noOK ='S'
005000041015     c                   exfmt     eu77d02
005100041015     C                   GOTO      FINE                                         F3=USCITA
005200041015     c                   end
005300041015      **
005400041013      ** Carico tutte le linee da gestire
005500041013     c                   clear                   linee
005600080416     c                   setoff                                       2928
005700091221     c********           if        %subst(Knmus:1:3) ='EDP'
005800091221     c********           seton                                        29
005900041013     c                   eval      linee(01) = 'DPD'
006000041013     c                   eval      linee(02) = 'FED'
006100091221     c*********          end
006200041013     c                   eval      linee(03) = '310'
006300041013     c                   eval      linee(04) = '320'
006400041013     c                   eval      linee(05) = '325'
006500041013     c                   eval      linee(06) = '330'
006600041013     c                   eval      linee(07) = '340'
006700041013     c                   eval      linee(08) = '345'
006800041013     c                   eval      linee(09) = '350'
006900041013     c                   eval      linee(10) = '360'
007000041013     c                   eval      linee(11) = '370'
007100100917     c                   eval      linee(12) = '375'
007200100917     c                   eval      linee(13) = '380'
007300100917     c                   eval      linee(14) = '390'
007400100917     c                   eval      linee(15) = '311'
007500120222     c                   eval      linee(16) = '333'
007600041013      **
007700040909     C     INIZIO        tag
007800040909     c                   clear                   D1NTW
007900080416     c                   clear                   D1NTWsav
008000040909     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
008100040909     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
008200050419     C                   move      'B'           D1tip                          tipo elaborazione
008300040909      *
008400040909      * Emetto il formato video
008500040909     C     Video1        TAG
008600080416     c                   eval      d1NtwSav = d1Ntw
008700040909     C                   SETOFF                                       90
008800040909     C                   EXFMT     eu77d01
008900040909      *
009000040909     C   KC              GOTO      FINE                                         F3=USCITA
009100040909      *
009200040909      * Eseguo i  controlli sui campi immessi a video
009300040909     C                   EXSR      CONTR
009400040909      *
009500040909      * Se non vi sono errori ma non e' premuto F6 emetto video
009600040909     C   90
009700040909     CorNkf              GOTO      Video1
009800040909      *
009900040909     C     Video2        TAG
010000040909     C                   SETOFF                                       90
010100040909      *
010200041013      *  Se scelti tutti
010300041013     c                   if        d1ntw = '999'
010400041013      *
010500041013     c                   do        20            nr                3 0
010600041013     c                   if        linee(nr) = *blank
010700041013     c                   leave
010800041013     c                   end
010900041013     c                   move      linee(nr)     d1ntw
011000041013     C                   EXSR      LANCIO
011100041013     c                   enddo
011200041013      *
011300041013     c                   else
011400041013      *
011500041013      * ESEGUE il caricamento
011600041013     C                   EXSR      LANCIO
011700041013     c                   end
011800041013      *
011900161298      *----------------------------------------------------------------
012000040909     c     FINE          tag
012100030620     c                   seton                                        LR
012200040909      *--------------------------------------------------------------*
012300040909      * Routine per i controlli sui campi del formato video          *
012400040909      *--------------------------------------------------------------*
012500040909     C     CONTR         BEGSR
012600040909      *
012700040909     c                   movea     '0000000000'  *in(41)
012800040909     c                   movea     '0000000000'  *in(51)
012900080416      * --------------
013000080416      * controllo il Network immesso
013100040909     C     d1ntw         ifeq      *blank
013200040909     C                   SETON                                        4190
013300040909     C                   GOTO      endct1
013400080416     c                   else
013500091221     c******             if        %subst(Knmus:1:3) <> 'EDP'
013600091221     c******             if        d1ntw = 'DPD' or d1ntw = 'FED'
013700091221     C******             SETON                                        4690
013800091221     C******             GOTO      endct1
013900091221     c******             end
014000091221     c******             end
014100040909     C                   End
014200040909      * --------------
014300080416      * controllo immissione dalla data o la protegge
014400080416      * --------------
014500080416     c                   If        d1NtwSav <> d1Ntw
014600080416     c                   clear                   UltimaData
014700080416     C/EXEC SQL
014800080416     C+ SELECT max(ECFAAS*10000+ecfmgs) INTO :UltimaData
014900080416     C+ FROM tnecf00f WHERE ecfntw= :d1ntw
015000080416     C/END-EXEC
015100080416     c                   setoff                                       28
015200080416     c                   exsr      ctrl_estratto
015300080416      *
015400080416      * Al primo cambio di Network  Pulisce le date e riemette il video
015500080416     c                   if        sqlcod < 0  or UltimaData = 0
015600080416     c                   clear                   d1dDal
015700080416     c                   clear                   d1Dal
015800080416     c                   goto      endct1
015900080416     c                   end
016000080416      *
016100080416      * protegge se presenti già dei dati sul Network
016200080416     c                   if        UltimaData > 0
016300091221     c******             if        %subst(Knmus:1:3) <>'EDP'
016400091221     c******             seton                                        28
016500091221     c******             end
016600080416     c     *iso          move      UltimaData    Data_Iso
016700080416     c                   adddur    1:*d          Data_Iso
016800080416     c                   clear                   d1dDal
016900080416     c     *dmy          move      Data_Iso      d1dDal
017000080416     c                   endIF
017100080416     c                   eval      d1Dal = d1dDal
017200080416     c                   endIF
017300080416      * --------------
017400040909      * controllo immissione delle date
017500040909     C                   Z-ADD     d1DDAL        G02DAT
017600040909     C                   MOVEL     *BLANK        G02ERR
017700040909     C                   CALL      'XSRDA8'
017800040909     C                   PARM                    WLBDAT
017900040909     C     G02ERR        IFEQ      '1'
018000040909     C                   SETON                                        44  90
018100040909     C                   GOTO      endct1
018200040909     C                   ENDIF
018300040909     C                   Z-ADD     G02INV        DalAmg            8 0
018400040909     C                   Z-ADD     G02dat        D1ddal
018500040909      *
018600040909      * Controllo data finale
018700040909     C     d1Dal         ifeq      0
018800040909     C                   Z-ADD     d1DDal        d1Dal
018900040909     C                   End
019000040909      *
019100040909     C                   Z-ADD     d1Dal         G02DAT
019200040909     C                   MOVEL     *BLANK        G02ERR
019300040909     C                   CALL      'XSRDA8'
019400040909     C                   PARM                    WLBDAT
019500040909     C     G02ERR        IFEQ      '1'
019600040909     C                   SETON                                        42  90
019700040909     C                   GOTO      endct1
019800040909     C                   ENDIF
019900040909     c*
020000040909     C                   Z-ADD     G02INV        AlAmG             8 0
020100040909     C                   Z-ADD     G02dat        d1dal
020200040909     c*
020300040909     C     DalAmg        ifgt      AlAmg
020400080416     c                   eval      alAmg = DalAmg
020500080416     c                   eval      d1dal = D1Ddal
020600080416     C*****              SETON                                        4590
020700080416     C*****              GOTO      endct1
020800040909     C                   END
020900040909      *
021000040909     C     endct1        ENDSR
021100040909      *--------------------------------------------------------------*
021200041013     C     Lancio        begSR
021300041013      *
021400041013     c                   clear                   pardate
021500041013      *
021600041013      * compone il codice numerico
021700041013     c                   move      d1ntw         Tpntw
021800041013     c                   z-add     DalAmg        DAdata
021900041013     c                   z-add     AlAmg         Adata
022000041013      **
022100041103     c                   clear                   kpjbu
022200041103     c                   movel     pardate       kpjbu
022300041103     c                   move      'EU78'        kcoaz
022400070619     c*******            move      'K214EDI'     kqebi
022500041013      *
022600041103      * Interattiva
022700041103     c                   if        d1tip = 'I'
022800041103     c                   call      'FIEU78R'
022900041103     c                   parm                    kpjba
023000041103      * Batch
023100041103     c                   else
023200091221     c                   if        %subst(Knmus:1:3) ='EDP'
023300041103     c                   call      'BCH09'
023400041103     c                   parm                    kpjba
023500080416     c                   end
023600041103     c                   call      'BCH10'
023700041103     c                   parm                    kpjba
023800041103     c                   end
023900041013      *
024000080416      * Aggiorna il file che tiene conto delle estrazioni effettuate
024100080416     c     d1ntw         chain     tnecx00f
024200080416     c                   eval      ECX_data_dal = DalAmg
024300080416     c                   eval      ECX_data_al  = AlAmg
024400080416     c                   eval      ECX_fatto_il  = *date
024500080416     c                   if        %Found(tnecx00f)
024600080416     c                   update    tnecx000
024700080416     c                   eLSe
024800080416     c                   eval      ECX_network = d1ntw
024900080416     c                   write     tnecx000
025000080416     c                   end
025100041013      *
025200041013     C                   ENDSR
025300041015      *--------------------------------------------------------------*
025400041015      *
025500041013      *--------------------------------------------------------------*
025600041015     C     CTR_tab_EVA   begSR
025700041015      *
025800041015     c                   clear                   kpjbu
025900041015     c                   clear                   tab_noOK          1
026000041015      *
026100041015     c                   movel     'C'           kpjbu
026200041015     c                   call      'FIEU90R'
026300041015     c                   parm                    kpjba
026400041015     c                   movel     kpjbu         tab_noOK
026500041015      *
026600041015     c                   if        tab_noOK = 'S'
026700041015      *
026800041015     c                   end
026900041015      *
027000041015     C                   ENDSR
027100041015      *--------------------------------------------------------------*
027200080416      *  Controlla l'ultima estrazione
027300080416      *--------------------------------------------------------------*
027400080416     C     ctrl_estratto begSR
027500080416      *
027600080416     c     d1ntw         chain     tnecx00f
027700080416     C                   Clear                   UltimaData_x
027800080416     c                   if        %Found(tnecx00f)
027900080416     C                   Z-ADD     ECX_data_al   UltimaData_x
028000080416     c                   end
028100080416      *
028200080416     c                   if        UltimaData_x > UltimaData
028300080416     c                   eval      UltimaData = UltimaData_x
028400080416     c                   end
028500080416      *
028600080416     C                   ENDSR
028700080416      *--------------------------------------------------------------*
