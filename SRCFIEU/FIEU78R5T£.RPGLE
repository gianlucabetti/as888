000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030627     FTita730C  iF   E           k DISK
000700030718     FEcCev30C  iF   E           k DISK
000800030718     FTneVA01L  iF   E           k DISK
000900030718     FTneVO01L  iF   E           k DISK
001000160203     FTNECF04L  uF a E           k DISK
001100030620      *
001200081022     D Scrivere        S              2
001300091001     D Importo         S             13  5
001400030630     D GRo             S              3    DIM(50) descend
001500030630     D GRp             S              3    DIM(50)
001600091001     D IMr             S             16  5 DIM(50)
001700091001     D IMc             S             16  5 DIM(50)
001800091001     D IMp             S             16  5 DIM(50)
001900040830     D Dfp             S              8  0 DIM(50)
002000040830     D Nfp             S             15    DIM(50)
002100030718      *
002200030721      * Importo Voci spedizione (0)
002300030721    >D EcceV0        E DS                  extname(eccev00f)
002400030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
002500030721     D CodV0           S              3  0 DIM(50)
002600030721     D                                     BASED(Ptc0)
002700030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
002800090910     D ImpV0           S             13  5 DIM(50)
002900030721     D                                     BASED(Pti0)
003000030721      * Importo Voci spedizione (P)
003100030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
003200030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003300030721     D CodVP           S              3  0 DIM(50)
003400030721     D                                     BASED(Ptcv)
003500030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
003600090910     D ImpVP           S             13  5 DIM(50)
003700030721     D                                     BASED(PtiV)
003800160203      *
003900030624     D KPJBA         E DS
004000030625      *
004100030625     D WLBDAT          DS
004200030625     D  G02DAT                 1      8  0
004300030625     D  G02INV                 9     16  0
004400030625     D  G02ERR                17     17
004500030625     D  G02TGI                18     22  0
004600040902      *
004700040902     D  data_Oggi      s                   like(G02INV) INZ(0)
004800030624      *
004900030624     D param           DS
005000030624     D   kAas                  1      4  0
005100030624     D   kLnp                  5      7  0
005200030624     D   kNrs                  8      9  0
005300030624     D   kNsp                 10     16  0
005400030624     D   kTbl                 17     18
005500030703     D   kNtw                 19     21
005600060303      *
005700050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
005800030721      *---------------------------------------------------------------------*
005900030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
006000030721     IECCEV000      01
006100030721     IECCEVP00      02
006200030620      *---------------------------------------------------------------------*
006300030624     C     *entry        plist
006400030624     C                   parm                    kpjba
006500050907     C                   parm                    DsTAS
006600050907      **** la DsTAS sostituisce la lettura dello stesso record del TITAS
006700030624     c                   movel     kpjbu         param
006800030627      *
006900160204     c                   seton                                        RT
007000160204      *
007100030718     c     kEVA          klist
007200030722     c                   kfld                    evaVAR
007300030722     c                   kfld                    evaNTW
007400030722     c                   kfld                    evaFIL
007500030718      *
007600030718     c     kEVO          klist
007700030722     c                   kfld                    evoNTW
007800030722     c                   kfld                    evoFIL
007900030721     c                   kfld                    evoVOC
008000030627      *
008100030627     c     ktas3         klist
008200030625     c                   kfld                    tasAas
008300030625     c                   kfld                    tasLnp
008400030625     c                   kfld                    tasNrs
008500030625     c                   kfld                    tasNsp
008600030625     c                   kfld                    tasTbl
008700040830      *
008800040830     c     kECF          klist
008900040830     c                   kfld                    tasAas
009000040830     c                   kfld                    tasLnp
009100040830     c                   kfld                    tasNrs
009200040830     c                   kfld                    tasNsp
009300040902      *
009400040902     c     kECF1         klist
009500040902     c                   kfld                    ecfntw
009600040902     c                   kfld                    ecfrif
009700081022      *
009800081022     c     kECF4         klist
009900081022     c                   kfld                    ecfntw
010000081022     c                   kfld                    ecfrif
010100081022     c                   kfld                    ecfgrp
010200030625      *
010300040830     c                   clear                   Gro
010400040830     c                   clear                   Grp
010500040830     c                   clear                   ImR
010600040830     c                   clear                   ImC
010700040830     c                   clear                   ImP
010800040830     c                   clear                   DfP
010900040830     c                   clear                   NfP
011000030627      *  imposta UDATE
011100030627     C                   move      *date         G02DAT
011200030627     C                   MOVE      *ZEROS        G02INV
011300030627     C                   MOVE      *BLANKS       G02ERR
011400030627     C                   CALL      'XSRDA8'
011500030627     C                   PARM                    WLBDAT
011600040902     C                   Z-ADD     G02INV        data_oggi
011700030627      *
011800160204     c                   clear                   tnecf000
011900160204     c                   eval        EcfTPR = 'ORM'
012000030627     c                   movel     tasAAS        ECFAAS
012100030627     c                   movel     tasLNP        ECFLNP
012200030627     c                   movel     tasNRS        ECFNRS
012300030627     c                   movel     tasNSP        ECFNSP
012400160204     c                   z-add     tasLNP        Linea_Cod         3 0
012500030627     c                   movel     tasLNA        ECFLNA
012600030627     c                   movel     tasMGS        ECFMGS
012700030627     c                   movel     tasTBL        ECFTBL
012800030627     c                   movel     tasFBR        ECFFBR
012900030627     c                   movel     tasKSC        ECFKSC
013000030627     c                   movel     tasDTI        ECFDTI
013100030627     c                   movel     tasDCM        ECFDCM
013200030627     c                   movel     tasCCA        ECFCCA
013300030627     c                   movel     tasNCL        ECFNCL
013400030627     c                   movel     tasPKF        ECFPKF
013500030627     c                   movel     tasVLF        ECFVLF
013600040902     c                   z-add     tasRMN        ECFRMN
013700030703      * dta e nr.fattura del Partner
013800030707     c                   movel     tasNFT        ECFNFT
013900030707     c                   movel     tasDFT        ECFDFT
014000160203     c                   move      tasRMN        fld14            14
014100160203     c                   movel(p)  fld14         ecfRIF
014200040902     C                   Z-ADD     data_oggi     ecfdta
014300030627      * Imposta i Ricavi
014400050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
014500050216     c                             tastbl <> 'AP'
014600030627     c                   exsr      Ricavi
014700050216     c                   end
014800030627      * Imposta le Competenze
014900030627     c                   exsr      Competenze
015000030627      *
015100030630      * riordina i dati immagazzinati per scrivere i records
015200030630     c                   movea     grp           gro
015300030630     c                   sorta     gro
015400030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
015500030630      *    nelle schiere
015600030630     c                   do        50            cnt               3 0
015700041015      *
015800041015     c                   select
015900160204     c                   when      gro(1)   = *blank and cnt = 1
016000041015     c                   move      '001'         gro(1)
016100041015     c                   when      gro(cnt) = *blank and cnt > 1
016200030630     c                   leave
016300041015     c                   endsl
016400030627      *
016500030723     c                   movel     gro(cnt)      ECFGRP
016600030630     c                   z-add     1             Ir
016700030630     c     gro(cnt)      lookup    Grp(Ir)                                21
016800030630      * Ricavi/Competenze
016900160204     c                   if        *in21
017000160204     c                   z-add(h)  ImR(Ir)       ECFIMR
017100160204     c                   z-add(h)  ImC(Ir)       ECFIMC
017200160204     c                   z-add(h)  ImP(Ir)       ECFIMP
017300160204     c                   z-add     DfP(Ir)       ECFDFP
017400160204     c                   movel     NfP(Ir)       ECFNFP
017500160204      *
017600081022      *  se presente lo stesso riferimento su altra bolla non ancora CONSOLIDATA
017700081022      *  si deve attribuire Ricavo e Competenza su quella altrimenti si scrive
017800160204     c     kECF4         chain     TNECF04L
017900160204     c                   if        %Found(TNECF04L) and ecfdtg=0
018000160204     c                   add       ImR(Ir)       ecfimR
018100160204     c                   add       ImC(Ir)       ecfimC
018200160204     c                   update    tnecf000
018300160204     c                   else
018400091022     c                   move(p)   1             EcfNFp
018500040830     c                   write     tnecf000
018600160204     c                   end
018700030627      *
018800160204     c                   end
018900160204      *
019000160204     c                   endDO
019100160204      *
019200160203     c                   RETURN
019300030627      *----------------------------------------------------------------
019400030627      *  imposta i Ricavi raggruppati su schiere
019500030627      *----------------------------------------------------------------
019600030627     c     Ricavi        Begsr
019700030627      *
019800030627      * IMPORTO Ricavo
019900030627      *  Porto
020000030627     c                   if        tasPOR > 0
020100030722     c                   clear                   evaVAR
020200030722     c                   movel     '1'           evaVAR
020300160126     c                   if        tasPOR  <> 8888888
020400030627     c                   z-add     tasPOR        importo
020500160126     c                   end
020600030718     c                   exsr      Tbe_EVA
020700030627     c                   end
020800030627      *  Varie 1
020900030627     c                   if        tasVA1 > 0
021000030722     c                   clear                   evaVAR
021100030722     c                   movel     tasSV1        evaVAR
021200160126     c                   if        tasVA1  <> 8888888
021300030627     c                   z-add     tasVA1        importo
021400160126     c                   end
021500030718     c                   exsr      Tbe_EVA
021600030627     c                   end
021700030627      *  Varie 2
021800030627     c                   if        tasVA2 > 0
021900030722     c                   clear                   evaVAR
022000030722     c                   movel     tasSV2        evaVAR
022100160126     c                   if        tasVA2  <> 8888888
022200030627     c                   z-add     tasVA2        importo
022300160126     c                   end
022400030718     c                   exsr      Tbe_EVA
022500030627     c                   end
022600030627      *  Varie 3
022700030627     c                   if        tasVA3 > 0
022800030722     c                   clear                   evaVAR
022900030722     c                   movel     tasSV3        evaVAR
023000160126     c                   if        tasVA3  <> 8888888
023100030627     c                   z-add     tasVA3        importo
023200160126     c                   end
023300030718     c                   exsr      Tbe_EVA
023400030627     c                   end
023500030627      *  Altre Varie
023600030627      *
023700030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
023800030627      *  e raggruppare.
023900030627     c     ktas3         setll     tita730c
024000030627     c     ktas3         reade     tita730c
024100030627     c                   dow       not %Eof(tita730c)
024200030722     c                   clear                   evaVAR
024300030722     c                   movel     ta7SVn        evaVAR
024400160126     c                   if        ta7VAn  <> 8888888
024500030627     c                   z-add     ta7VAn        importo
024600160126     c                   end
024700030718     c                   exsr      Tbe_EVA
024800030627     c     ktas3         reade     tita730c
024900030627     c                   enddo
025000030627      *
025100030627     c                   Endsr
025200030627      *----------------------------------------------------------------
025300030627      *  Decodifica il raggruppamento per le Varie
025400030627      *----------------------------------------------------------------
025500030718     c     TBE_EVA       Begsr
025600030627      *
025700030722     c                   clear                   evaNTW
025800030722     c                   clear                   evaFIL
025900030718     c                   move      '999'         Gruppo            3
026000030721      *
026100030721      * prova se ci sono delle particolarità sul Partner + LNP/A
026200030722      *  chiave completa
026300160203     c                   movel     kNtw          evaNTW
026400030722     c                   move      Linea_Cod     evaFIL
026500030721     c     kEVA          setll     tnEVA01l
026600030721     C                   IF        not %EQUAL
026700030721      * se ci sono dei codici particolari da prendere in considerazione
026800030627      *  al posto di quelli standard.
026900030722      *  chiave parziale
027000160203     c                   movel     kNtw          evaNTW
027100030722     c                   clear                   evaFIL
027200030718     c     kEVA          setll     tnEVA01l
027300030718     C                   IF        not %EQUAL
027400030722     c                   clear                   evaNTW
027500030722     c                   clear                   evaFIL
027600030722     c     kEVA          setll     tnEVA01l
027700030718     c                   end
027800030721     c                   endIF
027900030627      *
028000030718     C                   IF        %EQUAL
028100030718      *
028200030718     C                   DOU       %EOF
028300030718     C     kEVA          Reade     tnEVA01l
028400030718     C                   IF        NOT %EOF and
028500040902     C                             evaDDA <= data_oggi and
028600040902     C                             evaADA >= data_oggi
028700030718     c                   move      evaGRP        Gruppo
028800030718     c                   leave
028900030718     C                   ENDIF
029000030718     C                   ENDDO
029100030718      *
029200030718     c                   end
029300030718      *
029400030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
029500030718      *  legato alla tabella EVA.
029600030627     c                   z-add     1             Ir                3 0
029700030718     c     Gruppo        lookup    Grp(Ir)                                21
029800030627     c  n21              z-add     1             Ir
029900030630     c  n21*blank        lookup    Grp(Ir)                                21
030000030718     c                   movel     Gruppo        Grp(Ir)
030100030627     c                   add       Importo       ImR(Ir)
030200030627      *
030300030627     c                   Endsr
030400030627      *----------------------------------------------------------------
030500030627      *  imposta le Competenze raggruppate su schiere
030600030627      *----------------------------------------------------------------
030700030627     c     Competenze    Begsr
030800030627      *
030900030721     c                   clear                   ImpV0
031000030721     c                   clear                   ImpVP
031100030721     c                   clear                   CodV0
031200030721     c                   clear                   CodVP
031300160204      *
031400030627      * IMPORTO Competenze
031500030718     c     ktas3         chain     Eccev30C
031600030718     c                   if        %Found(Eccev30C)
031700030627      *
031800030721     C                   Do        50            CV                3 0
031900030721      *
032000030721     C                   If        *in01 = *on
032100030721      * esce quando non ha più Voci in skiera
032200030721     C                   If        CodV0(CV) = 0
032300030721     c                   leave
032400030721     c                   end
032500030721     c                   clear                   evoVOC
032600030721     c                   movel     CodV0(CV)     evoVOC
032700030721     c                   z-add     ImpV0(CV)     importo
032800030721     c                   exsr      Tbe_EVO
032900030721     c                   end
033000030718      *
033100030721     C                   If        *in02 = *on
033200030721      * esce quando non ha più Voci in skiera
033300030721     C                   If        CodVP(CV) = 0
033400030721     c                   leave
033500030721     c                   end
033600030721     c                   clear                   evoVOC
033700030721     c                   movel     CodVP(CV)     evoVOC
033800030721     c                   z-add     ImpVP(CV)     importo
033900030721     c                   exsr      Tbe_EVO
034000030721     c                   end
034100030721      *
034200030721     c                   endDo
034300030627      *
034400030721     C                   endIF
034500030721      *
034600030627     c                   Endsr
034700030627      *----------------------------------------------------------------
034800030627      *  Decodifica il raggruppamento per le Voci
034900030627      *----------------------------------------------------------------
035000030718     c     TBE_EVO       Begsr
035100030722      *
035200030722     c                   clear                   evoNTW
035300030722     c                   clear                   evoFIL
035400030722     c                   move      '999'         Gruppo            3
035500030722      *
035600030722      * prova se ci sono delle particolarità sul Partner + LNP/A
035700160203      *  chiave completa
035800160203     c                   movel     kNtw          evoNTW
035900030722     c                   move      Linea_Cod     evoFIL
036000030718     c     kEVO          setll     tnEVO01l
036100030718     C                   IF        not %EQUAL
036200030722      * se ci sono dei codici particolari da prendere in considerazione
036300030722      *  al posto di quelli standard.
036400030722      *  chiave parziale
036500160203     c                   movel     kNtw          evoNTW
036600030723     c                   clear                   evoFIL
036700030718     c     kEVO          setll     tnEVO01l
036800030718     C                   IF        not %EQUAL
036900030723     c                   clear                   evoNTW
037000030723     c                   clear                   evoFIL
037100030722     c     kEVO          setll     tnEVO01l
037200030718     c                   end
037300030718     c                   endIF
037400030718      *
037500030718     C                   IF        %EQUAL
037600030718      *
037700030718     C                   DOU       %EOF
037800030718     C     kEVO          Reade     tnEVO01l
037900030718     C                   IF        NOT %EOF        and
038000040902     C                             evoDDA <= data_oggi and
038100040902     C                             evoADA >= data_oggi
038200030718     c                   move      evoGRP        Gruppo
038300030718     c                   leave
038400030718     C                   ENDIF
038500030718     C                   ENDDO
038600030627      *
038700030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
038800030718      *  legato alla tabella EVO.
038900030627     c                   z-add     1             Ic                3 0
039000030718     c     Gruppo        lookup    Grp(Ic)                                21
039100030627     c  n21              z-add     1             Ic
039200030630     c  n21*blank        lookup    Grp(Ic)                                21
039300030718     c                   movel     Gruppo        Grp(Ic)
039400030627     c                   add       Importo       ImC(Ic)
039500030721      *
039600030721     c                   end
039700030703      *
039800030627     c                   Endsr
039900030627      *----------------------------------------------------------------
040000050907     c     *inzsr        Begsr
040100050907      *
040200050907      *
040300050907     c                   Endsr
040400050907      *----------------------------------------------------------------
