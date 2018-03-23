000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030624     FTitas30C  iF   E           k DISK
000700030627     FTita730C  iF   E           k DISK
000800041011     FAzOrg01L  iF   E           k DISK
000900030718     FEcCev30C  iF   E           k DISK
001000030718     FTneVA01L  iF   E           k DISK
001100030718     FTneVO01L  iF   E           k DISK
001200030620      *
001300091001     D Importo         S             13  5
001400030630     D GRo             S              3    DIM(50) descend
001500030630     D GRp             S              3    DIM(50)
001600091001     D IMr             S             16  5 DIM(50)
001700091001     D IMc             S             16  5 DIM(50)
001800030718      *
001900030721      * Importo Voci spedizione (0)
002000030721    >D EcceV0        E DS                  extname(eccev00f)
002100030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
002200030721     D CodV0           S              3  0 DIM(50)
002300030721     D                                     BASED(Ptc0)
002400030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
002500090910     D ImpV0           S             13  5 DIM(50)
002600030721     D                                     BASED(Pti0)
002700030721      * Importo Voci spedizione (P)
002800030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
002900030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003000030721     D CodVP           S              3  0 DIM(50)
003100030721     D                                     BASED(Ptcv)
003200030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
003300090910     D ImpVP           S             13  5 DIM(50)
003400030721     D                                     BASED(PtiV)
003500030627      *
003600030624     D KPJBA         E DS
003700030625     D OG143         E DS
003800030625      *
003900030625     D WLBDAT          DS
004000030625     D  G02DAT                 1      8  0
004100030625     D  G02INV                 9     16  0
004200030625     D  G02ERR                17     17
004300030625     D  G02TGI                18     22  0
004400040818      *
004500040818     D  data_Oggi      s                   like(G02INV) INZ(0)
004600040818      *
004700041011    >D tnecfds       E DS                  extname(tnecf00f)
004800041011      *
004900040818     D param           DS
005000041011     D   kAas                         4  0
005100041011     D   kLnp                         3  0
005200041011     D   kNrs                         2  0
005300041011     D   kNsp                         7  0
005400041011     D   kGrp                         3A
005500041011     D   kNft                         6  0
005600041011     D   kDft                         8  0
005700041011     D   kImr                        13  3
005800041011     D   kImc                        13  3
005900040827      *
006000030721      *---------------------------------------------------------------------*
006100030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
006200030721     IECCEV000      01
006300030721     IECCEVP00      02
006400030620      *---------------------------------------------------------------------*
006500030624     C     *entry        plist
006600030624     C                   parm                    kpjba
006700030624     c                   movel     kpjbu         param
006800040818      *
006900040818      *  imposta UDATE di elaborazione
007000040818     C                   move      *date         G02DAT
007100040818     C                   MOVE      *ZEROS        G02INV
007200040818     C                   MOVE      *BLANKS       G02ERR
007300040818     C                   CALL      'XSRDA8'
007400040818     C                   PARM                    WLBDAT
007500040818     C                   Z-ADD     G02INV        data_oggi
007600030718      *
007700030718     c     kEVA          klist
007800030722     c                   kfld                    evaVAR
007900030722     c                   kfld                    evaNTW
008000030722     c                   kfld                    evaFIL
008100030718      *
008200030718     c     kEVO          klist
008300030722     c                   kfld                    evoNTW
008400030722     c                   kfld                    evoFIL
008500030721     c                   kfld                    evoVOC
008600030627      *
008700030627     c     ktas1         klist
008800030627     c                   kfld                    kAas
008900030624     c                   kfld                    kLnp
009000030624     c                   kfld                    kNrs
009100030624     c                   kfld                    kNsp
009200030625      *
009300030627     c     ktas3         klist
009400030625     c                   kfld                    tasAas
009500030625     c                   kfld                    tasLnp
009600030625     c                   kfld                    tasNrs
009700030625     c                   kfld                    tasNsp
009800030625     c                   kfld                    tasTbl
009900040827      *
010000040827     c     ktas1         CHAIN     titas30c
010100040827     c                   IF        %FOUND(titas30c)
010200041011     c                   exsr      fld_FatBart
010300040827     C                   End
010400030624      *
010500041011     c                   movel     param         kpjbu
010600030620     c                   seton                                        LR
010700030627      *----------------------------------------------------------------
010800030627      * imposta i campi per la scrittura
010900030627      *----------------------------------------------------------------
011000041011     c     fld_FatBart   Begsr
011100030627      *
011200041011     c                   exsr      NetW
011300041011      *
011400030630     c                   clear                   Gro
011500030630     c                   clear                   Grp
011600030627     c                   clear                   ImR
011700030627     c                   clear                   ImC
011800030703      *
011900030703      * dta e nr.fattura del Partner
012000041011     c                   movel     tasNFT        kNFT
012100041011     c                   movel     tasDFT        kDFT
012200030630      *
012300030627      * Imposta i Ricavi
012400030627     c                   exsr      Ricavi
012500030627      * Imposta le Competenze
012600030627     c                   exsr      Competenze
012700030630      *
012800030630     c                   z-add     1             Ir
012900040827     c     kgrp          lookup    Grp(Ir)                                21
013000030630      * Ricavi/Competenze
013100041011     c   21              z-add     ImR(Ir)       kIMR
013200041011     c   21              z-add     ImC(Ir)       kIMC
013300030630      *
013400030627     c                   Endsr
013500030627      *----------------------------------------------------------------
013600030627      *  imposta i Ricavi raggruppati su schiere
013700030627      *----------------------------------------------------------------
013800030627     c     Ricavi        Begsr
013900030627      *
014000030627      * IMPORTO Ricavo
014100030627      *  Porto
014200030627     c                   if        tasPOR > 0
014300030722     c                   clear                   evaVAR
014400030722     c                   movel     '1'           evaVAR
014500030627     c                   z-add     tasPOR        importo
014600030718     c                   exsr      Tbe_EVA
014700030627     c                   end
014800030627      *  Varie 1
014900030627     c                   if        tasVA1 > 0
015000030722     c                   clear                   evaVAR
015100030722     c                   movel     tasSV1        evaVAR
015200030627     c                   z-add     tasVA1        importo
015300030718     c                   exsr      Tbe_EVA
015400030627     c                   end
015500030627      *  Varie 2
015600030627     c                   if        tasVA2 > 0
015700030722     c                   clear                   evaVAR
015800030722     c                   movel     tasSV2        evaVAR
015900030627     c                   z-add     tasVA2        importo
016000030718     c                   exsr      Tbe_EVA
016100030627     c                   end
016200030627      *  Varie 3
016300030627     c                   if        tasVA3 > 0
016400030722     c                   clear                   evaVAR
016500030722     c                   movel     tasSV3        evaVAR
016600030627     c                   z-add     tasVA3        importo
016700030718     c                   exsr      Tbe_EVA
016800030627     c                   end
016900030627      *  Altre Varie
017000030627      *
017100030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
017200030627      *  e raggruppare.
017300030627     c     ktas3         setll     tita730c
017400030627     c     ktas3         reade     tita730c
017500030627     c                   dow       not %Eof(tita730c)
017600030722     c                   clear                   evaVAR
017700030722     c                   movel     ta7SVn        evaVAR
017800030627     c                   z-add     ta7VAn        importo
017900030718     c                   exsr      Tbe_EVA
018000030627     c     ktas3         reade     tita730c
018100030627     c                   enddo
018200030627      *
018300030627     c                   Endsr
018400030627      *----------------------------------------------------------------
018500030627      *  Decodifica il raggruppamento per le Varie
018600030627      *----------------------------------------------------------------
018700030718     c     TBE_EVA       Begsr
018800030627      *
018900030722     c                   clear                   evaNTW
019000030722     c                   clear                   evaFIL
019100030718     c                   move      '999'         Gruppo            3
019200030721      *
019300030721      * prova se ci sono delle particolarità sul Partner + LNP/A
019400030722      *  chiave completa
019500030722     c                   movel     §ogNtw        evaNTW
019600030722     c                   move      Linea_Cod     evaFIL
019700030721     c     kEVA          setll     tnEVA01l
019800030721     C                   IF        not %EQUAL
019900030721      * se ci sono dei codici particolari da prendere in considerazione
020000030627      *  al posto di quelli standard.
020100030722      *  chiave parziale
020200030722     c                   movel     §ogNtw        evaNTW
020300030722     c                   clear                   evaFIL
020400030718     c     kEVA          setll     tnEVA01l
020500030718     C                   IF        not %EQUAL
020600030722     c                   clear                   evaNTW
020700030722     c                   clear                   evaFIL
020800030722     c     kEVA          setll     tnEVA01l
020900030718     c                   end
021000030721     c                   endIF
021100030627      *
021200030718     C                   IF        %EQUAL
021300030718      *
021400030718     C                   DOU       %EOF
021500030718     C     kEVA          Reade     tnEVA01l
021600030718     C                   IF        NOT %EOF and
021700041011     C                             evaDDA <= data_oggi and
021800041011     C                             evaADA >= data_oggi
021900030718     c                   move      evaGRP        Gruppo
022000030718     c                   leave
022100030718     C                   ENDIF
022200030718     C                   ENDDO
022300030718      *
022400030718     c                   end
022500030718      *
022600030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
022700030718      *  legato alla tabella EVA.
022800030627     c                   z-add     1             Ir                3 0
022900030718     c     Gruppo        lookup    Grp(Ir)                                21
023000030627     c  n21              z-add     1             Ir
023100030630     c  n21*blank        lookup    Grp(Ir)                                21
023200030718     c                   movel     Gruppo        Grp(Ir)
023300030627     c                   add       Importo       ImR(Ir)
023400030627      *
023500030627     c                   Endsr
023600030627      *----------------------------------------------------------------
023700030627      *  imposta le Competenze raggruppate su schiere
023800030627      *----------------------------------------------------------------
023900030627     c     Competenze    Begsr
024000030627      *
024100030721     c                   clear                   ImpV0
024200030721     c                   clear                   ImpVP
024300030721     c                   clear                   CodV0
024400030721     c                   clear                   CodVP
024500030721      *
024600030627      * IMPORTO Competenze
024700030718     c     ktas3         chain     Eccev30C
024800030718     c                   if        %Found(Eccev30C)
024900030627      *
025000030721     C                   Do        50            CV                3 0
025100030721      *
025200030721     C                   If        *in01 = *on
025300030721      * esce quando non ha più Voci in skiera
025400030721     C                   If        CodV0(CV) = 0
025500030721     c                   leave
025600030721     c                   end
025700030721     c                   clear                   evoVOC
025800030721     c                   movel     CodV0(CV)     evoVOC
025900030721     c                   z-add     ImpV0(CV)     importo
026000030721     c                   exsr      Tbe_EVO
026100030721     c                   end
026200030718      *
026300030721     C                   If        *in02 = *on
026400030721      * esce quando non ha più Voci in skiera
026500030721     C                   If        CodVP(CV) = 0
026600030721     c                   leave
026700030721     c                   end
026800030721     c                   clear                   evoVOC
026900030721     c                   movel     CodVP(CV)     evoVOC
027000030721     c                   z-add     ImpVP(CV)     importo
027100030721     c                   exsr      Tbe_EVO
027200030721     c                   end
027300030721      *
027400030721     c                   endDo
027500030627      *
027600030721     C                   endIF
027700030721      *
027800030627     c                   Endsr
027900030627      *----------------------------------------------------------------
028000030627      *  Decodifica il raggruppamento per le Voci
028100030627      *----------------------------------------------------------------
028200030718     c     TBE_EVO       Begsr
028300030722      *
028400030722     c                   clear                   evoNTW
028500030722     c                   clear                   evoFIL
028600030722     c                   move      '999'         Gruppo            3
028700030722      *
028800030722      * prova se ci sono delle particolarità sul Partner + LNP/A
028900030722      *  chiave completa
029000030722     c                   movel     §ogNtw        evoNTW
029100030722     c                   move      Linea_Cod     evoFIL
029200030718     c     kEVO          setll     tnEVO01l
029300030718     C                   IF        not %EQUAL
029400030722      * se ci sono dei codici particolari da prendere in considerazione
029500030722      *  al posto di quelli standard.
029600030722      *  chiave parziale
029700030723     c                   movel     §ogNtw        evoNTW
029800030723     c                   clear                   evoFIL
029900030718     c     kEVO          setll     tnEVO01l
030000030718     C                   IF        not %EQUAL
030100030723     c                   clear                   evoNTW
030200030723     c                   clear                   evoFIL
030300030722     c     kEVO          setll     tnEVO01l
030400030718     c                   end
030500030718     c                   endIF
030600030718      *
030700030718     C                   IF        %EQUAL
030800030718      *
030900030718     C                   DOU       %EOF
031000030718     C     kEVO          Reade     tnEVO01l
031100030718     C                   IF        NOT %EOF        and
031200041011     C                             evoDDA <= data_oggi and
031300041011     C                             evoADA >= data_oggi
031400030718     c                   move      evoGRP        Gruppo
031500030718     c                   leave
031600030718     C                   ENDIF
031700030718     C                   ENDDO
031800030627      *
031900030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
032000030718      *  legato alla tabella EVO.
032100030627     c                   z-add     1             Ic                3 0
032200030718     c     Gruppo        lookup    Grp(Ic)                                21
032300030627     c  n21              z-add     1             Ic
032400030630     c  n21*blank        lookup    Grp(Ic)                                21
032500030718     c                   movel     Gruppo        Grp(Ic)
032600030627     c                   add       Importo       ImC(Ic)
032700030721      *
032800030721     c                   end
032900030703      *
033000030627     c                   Endsr
033100041011      *----------------------------------------------------------------
033200041011      *  Decodifica il Network
033300041011      *----------------------------------------------------------------
033400041011     c     Netw          Begsr
033500041011      *
033600041011     c                   clear                   ImpExp            1
033700041011      *
033800041011      *  x LNP controlla Estero
033900041011     C                   clear                   OG143
034000041011     c     tasLNP        chain     AzOrg01L
034100041011     c                   if        %Found(AzOrg01L)
034200041011     C                   movel     ORGDE3        OG143
034300041011     c                   end
034400041011      *
034500041011      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
034600041011      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
034700041011      *  se non fosse risultato tale allora deve controllare x LNA.
034800041011     c                   if         §ogNTW = 'FED' or
034900041011     c                              §ogNTW = 'EEX' or
035000041011     c                              §ogNTW = 'DPD'
035100041011      *  quindi le competenze da tenere in considerazione per il confronto sono
035200041011      *  di arrivo.
035300041011     c                   movel     'I'           ImpExp
035400041011     c                   movel     'LNP'         Estero_LN         3
035500041011     c                   z-add     tasLNP        Linea_Cod         3 0
035600041011      *
035700041011     c                   else
035800041011     C                   clear                   OG143
035900041011      * prova per LNA -> se prima, x LNP il network non è un estero
036000041011     c     tasLNA        chain     AzOrg01L
036100041011     c                   if        %Found(AzOrg01L)
036200041011     C                   movel     ORGDE3        OG143
036300041011      *  quindi le competenze da tenere in considerazione per il confronto sono
036400041011      *  di partenza.
036500041011     c                   movel     'E'           ImpExp
036600041011     c                   movel     'LNA'         Estero_LN
036700041011     c                   z-add     tasLNA        Linea_Cod
036800041011     c                   end
036900041011     c                   end
037000041011      *
037100041011     c                   Endsr
037200030627      *----------------------------------------------------------------
