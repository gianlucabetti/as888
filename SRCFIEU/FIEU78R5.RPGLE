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
005800160205     D DsDPO         E DS                  EXTNAME(FIDPO00F)
005900160205     D DsTA4         E DS                  EXTNAME(TITA400F)
006000030721      *---------------------------------------------------------------------*
006100030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
006200030721     IECCEV000      01
006300030721     IECCEVP00      02
006400030620      *---------------------------------------------------------------------*
006500030624     C     *entry        plist
006600030624     C                   parm                    kpjba
006700050907     C                   parm                    DsTAS
006800160205     C                   parm                    DsDPO
006900050907      **** la DsTAS sostituisce la lettura dello stesso record del TITAS
007000030624     c                   movel     kpjbu         param
007100030627      *
007200160204     c                   seton                                        RT
007300160204      *
007400030718     c     kEVA          klist
007500030722     c                   kfld                    evaVAR
007600030722     c                   kfld                    evaNTW
007700030722     c                   kfld                    evaFIL
007800030718      *
007900030718     c     kEVO          klist
008000030722     c                   kfld                    evoNTW
008100030722     c                   kfld                    evoFIL
008200030721     c                   kfld                    evoVOC
008300030627      *
008400030627     c     ktas3         klist
008500030625     c                   kfld                    tasAas
008600030625     c                   kfld                    tasLnp
008700030625     c                   kfld                    tasNrs
008800030625     c                   kfld                    tasNsp
008900030625     c                   kfld                    tasTbl
009000040830      *
009100040830     c     kECF          klist
009200040830     c                   kfld                    tasAas
009300040830     c                   kfld                    tasLnp
009400040830     c                   kfld                    tasNrs
009500040830     c                   kfld                    tasNsp
009600040902      *
009700040902     c     kECF1         klist
009800040902     c                   kfld                    ecfntw
009900040902     c                   kfld                    ecfrif
010000081022      *
010100081022     c     kECF4         klist
010200081022     c                   kfld                    ecfntw
010300081022     c                   kfld                    ecfrif
010400081022     c                   kfld                    ecfgrp
010500030625      *
010600040830     c                   clear                   Gro
010700040830     c                   clear                   Grp
010800040830     c                   clear                   ImR
010900040830     c                   clear                   ImC
011000040830     c                   clear                   ImP
011100040830     c                   clear                   DfP
011200040830     c                   clear                   NfP
011300030627      *  imposta UDATE
011400030627     C                   move      *date         G02DAT
011500030627     C                   MOVE      *ZEROS        G02INV
011600030627     C                   MOVE      *BLANKS       G02ERR
011700030627     C                   CALL      'XSRDA8'
011800030627     C                   PARM                    WLBDAT
011900040902     C                   Z-ADD     G02INV        data_oggi
012000030627      *
012100160204     c                   clear                   tnecf000
012200160204     c                   eval        EcfTPR = 'ORB'
012300160204     c                   eval        EcfNTW = 'DPD'
012400030627     c                   movel     tasAAS        ECFAAS
012500030627     c                   movel     tasLNP        ECFLNP
012600030627     c                   movel     tasNRS        ECFNRS
012700030627     c                   movel     tasNSP        ECFNSP
012800160204     c                   z-add     tasLNP        Linea_Cod         3 0
012900030627     c                   movel     tasLNA        ECFLNA
013000030627     c                   movel     tasMGS        ECFMGS
013100030627     c                   movel     tasTBL        ECFTBL
013200030627     c                   movel     tasFBR        ECFFBR
013300030627     c                   movel     tasKSC        ECFKSC
013400030627     c                   movel     tasDTI        ECFDTI
013500030627     c                   movel     tasDCM        ECFDCM
013600030627     c                   movel     tasCCA        ECFCCA
013700030627     c                   movel     tasNCL        ECFNCL
013800030627     c                   movel     tasPKF        ECFPKF
013900030627     c                   movel     tasVLF        ECFVLF
014000040902     c                   z-add     tasRMN        ECFRMN
014100160527     c**
014200160527     c                   if        tasNZD <>*blank
014300160527     c                   movel     tasNZD        ECFNAZ
014400160527     c                   elseIf    tasNZM <>*blank
014500160527     c                   movel     tasNZM        ECFNAZ
014600160527     c                   end
014700160527     c**
014800160527     c                   movel     tasCTS        ECFCTS
014900160527     c                   movel     tasFIN        ECFFIN
015000160527     c                   movel     tasFTC        ECFFTC
015100160527     c                   movel     tasTC2        ECFTC2
015200160527      *
015300030703      * dta e nr.fattura del Partner
015400030707     c                   movel     tasNFT        ECFNFT
015500030707     c                   movel     tasDFT        ECFDFT
015600160205     c                   movel     dpoNRP        ecfRIF
015700030627      * Imposta i Ricavi
015800050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
015900050216     c                             tastbl <> 'AP'
016000030627     c                   exsr      Ricavi
016100050216     c                   end
016200030627      * Imposta le Competenze
016300030627     c                   exsr      Competenze
016400030627      *
016500030630      * riordina i dati immagazzinati per scrivere i records
016600030630     c                   movea     grp           gro
016700030630     c                   sorta     gro
016800030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
016900030630      *    nelle schiere
017000030630     c                   do        50            cnt               3 0
017100041015      *
017200041015     c                   select
017300160204     c                   when      gro(1)   = *blank and cnt = 1
017400041015     c                   move      '001'         gro(1)
017500041015     c                   when      gro(cnt) = *blank and cnt > 1
017600030630     c                   leave
017700041015     c                   endsl
017800030627      *
017900030723     c                   movel     gro(cnt)      ECFGRP
018000030630     c                   z-add     1             Ir
018100030630     c     gro(cnt)      lookup    Grp(Ir)                                21
018200030630      * Ricavi/Competenze
018300160204     c                   if        *in21
018400160204     c                   z-add(h)  ImR(Ir)       ECFIMR
018500160204     c                   z-add(h)  ImC(Ir)       ECFIMC
018600160204     c                   z-add(h)  ImP(Ir)       ECFIMP
018700160204     c                   z-add     DfP(Ir)       ECFDFP
018800160204     c                   movel     NfP(Ir)       ECFNFP
018900160204      *
019000081022      *  se presente lo stesso riferimento su altra bolla non ancora CONSOLIDATA
019100081022      *  si deve attribuire Ricavo e Competenza su quella altrimenti si scrive
019200160204     c     kECF4         chain     TNECF04L
019300160204     c                   if        %Found(TNECF04L) and ecfdtg=0
019400160204     c                   add       ImR(Ir)       ecfimR
019500160204     c                   add       ImC(Ir)       ecfimC
019600160204     c                   update    tnecf000
019700160204     c                   else
019800091022     c                   move(p)   1             EcfNFp
019900040830     c                   write     tnecf000
020000160204     c                   end
020100030627      *
020200160204     c                   end
020300160204      *
020400160204     c                   endDO
020500160204      *
020600160203     c                   RETURN
020700030627      *----------------------------------------------------------------
020800030627      *  imposta i Ricavi raggruppati su schiere
020900030627      *----------------------------------------------------------------
021000030627     c     Ricavi        Begsr
021100030627      *
021200030627      * IMPORTO Ricavo
021300030627      *  Porto
021400030627     c                   if        tasPOR > 0
021500030722     c                   clear                   evaVAR
021600030722     c                   movel     '1'           evaVAR
021700160126     c                   if        tasPOR  <> 8888888
021800030627     c                   z-add     tasPOR        importo
021900160126     c                   end
022000030718     c                   exsr      Tbe_EVA
022100030627     c                   end
022200030627      *  Varie 1
022300030627     c                   if        tasVA1 > 0
022400030722     c                   clear                   evaVAR
022500030722     c                   movel     tasSV1        evaVAR
022600160126     c                   if        tasVA1  <> 8888888
022700030627     c                   z-add     tasVA1        importo
022800160126     c                   end
022900030718     c                   exsr      Tbe_EVA
023000030627     c                   end
023100030627      *  Varie 2
023200030627     c                   if        tasVA2 > 0
023300030722     c                   clear                   evaVAR
023400030722     c                   movel     tasSV2        evaVAR
023500160126     c                   if        tasVA2  <> 8888888
023600030627     c                   z-add     tasVA2        importo
023700160126     c                   end
023800030718     c                   exsr      Tbe_EVA
023900030627     c                   end
024000030627      *  Varie 3
024100030627     c                   if        tasVA3 > 0
024200030722     c                   clear                   evaVAR
024300030722     c                   movel     tasSV3        evaVAR
024400160126     c                   if        tasVA3  <> 8888888
024500030627     c                   z-add     tasVA3        importo
024600160126     c                   end
024700030718     c                   exsr      Tbe_EVA
024800030627     c                   end
024900030627      *  Altre Varie
025000030627      *
025100030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
025200030627      *  e raggruppare.
025300030627     c     ktas3         setll     tita730c
025400030627     c     ktas3         reade     tita730c
025500030627     c                   dow       not %Eof(tita730c)
025600030722     c                   clear                   evaVAR
025700030722     c                   movel     ta7SVn        evaVAR
025800160126     c                   if        ta7VAn  <> 8888888
025900030627     c                   z-add     ta7VAn        importo
026000160126     c                   end
026100030718     c                   exsr      Tbe_EVA
026200030627     c     ktas3         reade     tita730c
026300030627     c                   enddo
026400030627      *
026500030627     c                   Endsr
026600030627      *----------------------------------------------------------------
026700030627      *  Decodifica il raggruppamento per le Varie
026800030627      *----------------------------------------------------------------
026900030718     c     TBE_EVA       Begsr
027000030627      *
027100030722     c                   clear                   evaNTW
027200030722     c                   clear                   evaFIL
027300030718     c                   move      '999'         Gruppo            3
027400030721      *
027500030721      * prova se ci sono delle particolarità sul Partner + LNP/A
027600030722      *  chiave completa
027700160203     c                   movel     kNtw          evaNTW
027800030722     c                   move      Linea_Cod     evaFIL
027900030721     c     kEVA          setll     tnEVA01l
028000030721     C                   IF        not %EQUAL
028100030721      * se ci sono dei codici particolari da prendere in considerazione
028200030627      *  al posto di quelli standard.
028300030722      *  chiave parziale
028400160203     c                   movel     kNtw          evaNTW
028500030722     c                   clear                   evaFIL
028600030718     c     kEVA          setll     tnEVA01l
028700030718     C                   IF        not %EQUAL
028800030722     c                   clear                   evaNTW
028900030722     c                   clear                   evaFIL
029000030722     c     kEVA          setll     tnEVA01l
029100030718     c                   end
029200030721     c                   endIF
029300030627      *
029400030718     C                   IF        %EQUAL
029500030718      *
029600030718     C                   DOU       %EOF
029700030718     C     kEVA          Reade     tnEVA01l
029800030718     C                   IF        NOT %EOF and
029900040902     C                             evaDDA <= data_oggi and
030000040902     C                             evaADA >= data_oggi
030100030718     c                   move      evaGRP        Gruppo
030200030718     c                   leave
030300030718     C                   ENDIF
030400030718     C                   ENDDO
030500030718      *
030600030718     c                   end
030700030718      *
030800030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
030900030718      *  legato alla tabella EVA.
031000030627     c                   z-add     1             Ir                3 0
031100030718     c     Gruppo        lookup    Grp(Ir)                                21
031200030627     c  n21              z-add     1             Ir
031300030630     c  n21*blank        lookup    Grp(Ir)                                21
031400030718     c                   movel     Gruppo        Grp(Ir)
031500030627     c                   add       Importo       ImR(Ir)
031600030627      *
031700030627     c                   Endsr
031800030627      *----------------------------------------------------------------
031900030627      *  imposta le Competenze raggruppate su schiere
032000030627      *----------------------------------------------------------------
032100030627     c     Competenze    Begsr
032200030627      *
032300030721     c                   clear                   ImpV0
032400030721     c                   clear                   ImpVP
032500030721     c                   clear                   CodV0
032600030721     c                   clear                   CodVP
032700160204      *
032800030627      * IMPORTO Competenze
032900030718     c     ktas3         chain     Eccev30C
033000030718     c                   if        %Found(Eccev30C)
033100030627      *
033200030721     C                   Do        50            CV                3 0
033300030721      *
033400030721     C                   If        *in01 = *on
033500030721      * esce quando non ha più Voci in skiera
033600030721     C                   If        CodV0(CV) = 0
033700030721     c                   leave
033800030721     c                   end
033900030721     c                   clear                   evoVOC
034000030721     c                   movel     CodV0(CV)     evoVOC
034100030721     c                   z-add     ImpV0(CV)     importo
034200030721     c                   exsr      Tbe_EVO
034300030721     c                   end
034400030718      *
034500030721     C                   If        *in02 = *on
034600030721      * esce quando non ha più Voci in skiera
034700030721     C                   If        CodVP(CV) = 0
034800030721     c                   leave
034900030721     c                   end
035000030721     c                   clear                   evoVOC
035100030721     c                   movel     CodVP(CV)     evoVOC
035200030721     c                   z-add     ImpVP(CV)     importo
035300030721     c                   exsr      Tbe_EVO
035400030721     c                   end
035500030721      *
035600030721     c                   endDo
035700030627      *
035800030721     C                   endIF
035900030721      *
036000030627     c                   Endsr
036100030627      *----------------------------------------------------------------
036200030627      *  Decodifica il raggruppamento per le Voci
036300030627      *----------------------------------------------------------------
036400030718     c     TBE_EVO       Begsr
036500030722      *
036600030722     c                   clear                   evoNTW
036700030722     c                   clear                   evoFIL
036800030722     c                   move      '999'         Gruppo            3
036900030722      *
037000030722      * prova se ci sono delle particolarità sul Partner + LNP/A
037100160203      *  chiave completa
037200160203     c                   movel     kNtw          evoNTW
037300030722     c                   move      Linea_Cod     evoFIL
037400030718     c     kEVO          setll     tnEVO01l
037500030718     C                   IF        not %EQUAL
037600030722      * se ci sono dei codici particolari da prendere in considerazione
037700030722      *  al posto di quelli standard.
037800030722      *  chiave parziale
037900160203     c                   movel     kNtw          evoNTW
038000030723     c                   clear                   evoFIL
038100030718     c     kEVO          setll     tnEVO01l
038200030718     C                   IF        not %EQUAL
038300030723     c                   clear                   evoNTW
038400030723     c                   clear                   evoFIL
038500030722     c     kEVO          setll     tnEVO01l
038600030718     c                   end
038700030718     c                   endIF
038800030718      *
038900030718     C                   IF        %EQUAL
039000030718      *
039100030718     C                   DOU       %EOF
039200030718     C     kEVO          Reade     tnEVO01l
039300030718     C                   IF        NOT %EOF        and
039400040902     C                             evoDDA <= data_oggi and
039500040902     C                             evoADA >= data_oggi
039600030718     c                   move      evoGRP        Gruppo
039700030718     c                   leave
039800030718     C                   ENDIF
039900030718     C                   ENDDO
040000030627      *
040100030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
040200030718      *  legato alla tabella EVO.
040300030627     c                   z-add     1             Ic                3 0
040400030718     c     Gruppo        lookup    Grp(Ic)                                21
040500030627     c  n21              z-add     1             Ic
040600030630     c  n21*blank        lookup    Grp(Ic)                                21
040700030718     c                   movel     Gruppo        Grp(Ic)
040800030627     c                   add       Importo       ImC(Ic)
040900030721      *
041000030721     c                   end
041100030703      *
041200030627     c                   Endsr
041300030627      *----------------------------------------------------------------
041400050907     c     *inzsr        Begsr
041500050907      *
041600050907      *
041700050907     c                   Endsr
041800050907      *----------------------------------------------------------------
