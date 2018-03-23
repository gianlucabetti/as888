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
014100030703      * dta e nr.fattura del Partner
014200030707     c                   movel     tasNFT        ECFNFT
014300030707     c                   movel     tasDFT        ECFDFT
014400160205     c                   movel     dpoNRP        ecfRIF
014500030627      * Imposta i Ricavi
014600050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
014700050216     c                             tastbl <> 'AP'
014800030627     c                   exsr      Ricavi
014900050216     c                   end
015000030627      * Imposta le Competenze
015100030627     c                   exsr      Competenze
015200030627      *
015300030630      * riordina i dati immagazzinati per scrivere i records
015400030630     c                   movea     grp           gro
015500030630     c                   sorta     gro
015600030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
015700030630      *    nelle schiere
015800030630     c                   do        50            cnt               3 0
015900041015      *
016000041015     c                   select
016100160204     c                   when      gro(1)   = *blank and cnt = 1
016200041015     c                   move      '001'         gro(1)
016300041015     c                   when      gro(cnt) = *blank and cnt > 1
016400030630     c                   leave
016500041015     c                   endsl
016600030627      *
016700030723     c                   movel     gro(cnt)      ECFGRP
016800030630     c                   z-add     1             Ir
016900030630     c     gro(cnt)      lookup    Grp(Ir)                                21
017000030630      * Ricavi/Competenze
017100160204     c                   if        *in21
017200160204     c                   z-add(h)  ImR(Ir)       ECFIMR
017300160204     c                   z-add(h)  ImC(Ir)       ECFIMC
017400160204     c                   z-add(h)  ImP(Ir)       ECFIMP
017500160204     c                   z-add     DfP(Ir)       ECFDFP
017600160204     c                   movel     NfP(Ir)       ECFNFP
017700160204      *
017800081022      *  se presente lo stesso riferimento su altra bolla non ancora CONSOLIDATA
017900081022      *  si deve attribuire Ricavo e Competenza su quella altrimenti si scrive
018000160204     c     kECF4         chain     TNECF04L
018100160204     c                   if        %Found(TNECF04L) and ecfdtg=0
018200160204     c                   add       ImR(Ir)       ecfimR
018300160204     c                   add       ImC(Ir)       ecfimC
018400160204     c                   update    tnecf000
018500160204     c                   else
018600091022     c                   move(p)   1             EcfNFp
018700040830     c                   write     tnecf000
018800160204     c                   end
018900030627      *
019000160204     c                   end
019100160204      *
019200160204     c                   endDO
019300160204      *
019400160203     c                   RETURN
019500030627      *----------------------------------------------------------------
019600030627      *  imposta i Ricavi raggruppati su schiere
019700030627      *----------------------------------------------------------------
019800030627     c     Ricavi        Begsr
019900030627      *
020000030627      * IMPORTO Ricavo
020100030627      *  Porto
020200030627     c                   if        tasPOR > 0
020300030722     c                   clear                   evaVAR
020400030722     c                   movel     '1'           evaVAR
020500160126     c                   if        tasPOR  <> 8888888
020600030627     c                   z-add     tasPOR        importo
020700160126     c                   end
020800030718     c                   exsr      Tbe_EVA
020900030627     c                   end
021000030627      *  Varie 1
021100030627     c                   if        tasVA1 > 0
021200030722     c                   clear                   evaVAR
021300030722     c                   movel     tasSV1        evaVAR
021400160126     c                   if        tasVA1  <> 8888888
021500030627     c                   z-add     tasVA1        importo
021600160126     c                   end
021700030718     c                   exsr      Tbe_EVA
021800030627     c                   end
021900030627      *  Varie 2
022000030627     c                   if        tasVA2 > 0
022100030722     c                   clear                   evaVAR
022200030722     c                   movel     tasSV2        evaVAR
022300160126     c                   if        tasVA2  <> 8888888
022400030627     c                   z-add     tasVA2        importo
022500160126     c                   end
022600030718     c                   exsr      Tbe_EVA
022700030627     c                   end
022800030627      *  Varie 3
022900030627     c                   if        tasVA3 > 0
023000030722     c                   clear                   evaVAR
023100030722     c                   movel     tasSV3        evaVAR
023200160126     c                   if        tasVA3  <> 8888888
023300030627     c                   z-add     tasVA3        importo
023400160126     c                   end
023500030718     c                   exsr      Tbe_EVA
023600030627     c                   end
023700030627      *  Altre Varie
023800030627      *
023900030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
024000030627      *  e raggruppare.
024100030627     c     ktas3         setll     tita730c
024200030627     c     ktas3         reade     tita730c
024300030627     c                   dow       not %Eof(tita730c)
024400030722     c                   clear                   evaVAR
024500030722     c                   movel     ta7SVn        evaVAR
024600160126     c                   if        ta7VAn  <> 8888888
024700030627     c                   z-add     ta7VAn        importo
024800160126     c                   end
024900030718     c                   exsr      Tbe_EVA
025000030627     c     ktas3         reade     tita730c
025100030627     c                   enddo
025200030627      *
025300030627     c                   Endsr
025400030627      *----------------------------------------------------------------
025500030627      *  Decodifica il raggruppamento per le Varie
025600030627      *----------------------------------------------------------------
025700030718     c     TBE_EVA       Begsr
025800030627      *
025900030722     c                   clear                   evaNTW
026000030722     c                   clear                   evaFIL
026100030718     c                   move      '999'         Gruppo            3
026200030721      *
026300030721      * prova se ci sono delle particolarità sul Partner + LNP/A
026400030722      *  chiave completa
026500160203     c                   movel     kNtw          evaNTW
026600030722     c                   move      Linea_Cod     evaFIL
026700030721     c     kEVA          setll     tnEVA01l
026800030721     C                   IF        not %EQUAL
026900030721      * se ci sono dei codici particolari da prendere in considerazione
027000030627      *  al posto di quelli standard.
027100030722      *  chiave parziale
027200160203     c                   movel     kNtw          evaNTW
027300030722     c                   clear                   evaFIL
027400030718     c     kEVA          setll     tnEVA01l
027500030718     C                   IF        not %EQUAL
027600030722     c                   clear                   evaNTW
027700030722     c                   clear                   evaFIL
027800030722     c     kEVA          setll     tnEVA01l
027900030718     c                   end
028000030721     c                   endIF
028100030627      *
028200030718     C                   IF        %EQUAL
028300030718      *
028400030718     C                   DOU       %EOF
028500030718     C     kEVA          Reade     tnEVA01l
028600030718     C                   IF        NOT %EOF and
028700040902     C                             evaDDA <= data_oggi and
028800040902     C                             evaADA >= data_oggi
028900030718     c                   move      evaGRP        Gruppo
029000030718     c                   leave
029100030718     C                   ENDIF
029200030718     C                   ENDDO
029300030718      *
029400030718     c                   end
029500030718      *
029600030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
029700030718      *  legato alla tabella EVA.
029800030627     c                   z-add     1             Ir                3 0
029900030718     c     Gruppo        lookup    Grp(Ir)                                21
030000030627     c  n21              z-add     1             Ir
030100030630     c  n21*blank        lookup    Grp(Ir)                                21
030200030718     c                   movel     Gruppo        Grp(Ir)
030300030627     c                   add       Importo       ImR(Ir)
030400030627      *
030500030627     c                   Endsr
030600030627      *----------------------------------------------------------------
030700030627      *  imposta le Competenze raggruppate su schiere
030800030627      *----------------------------------------------------------------
030900030627     c     Competenze    Begsr
031000030627      *
031100030721     c                   clear                   ImpV0
031200030721     c                   clear                   ImpVP
031300030721     c                   clear                   CodV0
031400030721     c                   clear                   CodVP
031500160204      *
031600030627      * IMPORTO Competenze
031700030718     c     ktas3         chain     Eccev30C
031800030718     c                   if        %Found(Eccev30C)
031900030627      *
032000030721     C                   Do        50            CV                3 0
032100030721      *
032200030721     C                   If        *in01 = *on
032300030721      * esce quando non ha più Voci in skiera
032400030721     C                   If        CodV0(CV) = 0
032500030721     c                   leave
032600030721     c                   end
032700030721     c                   clear                   evoVOC
032800030721     c                   movel     CodV0(CV)     evoVOC
032900030721     c                   z-add     ImpV0(CV)     importo
033000030721     c                   exsr      Tbe_EVO
033100030721     c                   end
033200030718      *
033300030721     C                   If        *in02 = *on
033400030721      * esce quando non ha più Voci in skiera
033500030721     C                   If        CodVP(CV) = 0
033600030721     c                   leave
033700030721     c                   end
033800030721     c                   clear                   evoVOC
033900030721     c                   movel     CodVP(CV)     evoVOC
034000030721     c                   z-add     ImpVP(CV)     importo
034100030721     c                   exsr      Tbe_EVO
034200030721     c                   end
034300030721      *
034400030721     c                   endDo
034500030627      *
034600030721     C                   endIF
034700030721      *
034800030627     c                   Endsr
034900030627      *----------------------------------------------------------------
035000030627      *  Decodifica il raggruppamento per le Voci
035100030627      *----------------------------------------------------------------
035200030718     c     TBE_EVO       Begsr
035300030722      *
035400030722     c                   clear                   evoNTW
035500030722     c                   clear                   evoFIL
035600030722     c                   move      '999'         Gruppo            3
035700030722      *
035800030722      * prova se ci sono delle particolarità sul Partner + LNP/A
035900160203      *  chiave completa
036000160203     c                   movel     kNtw          evoNTW
036100030722     c                   move      Linea_Cod     evoFIL
036200030718     c     kEVO          setll     tnEVO01l
036300030718     C                   IF        not %EQUAL
036400030722      * se ci sono dei codici particolari da prendere in considerazione
036500030722      *  al posto di quelli standard.
036600030722      *  chiave parziale
036700160203     c                   movel     kNtw          evoNTW
036800030723     c                   clear                   evoFIL
036900030718     c     kEVO          setll     tnEVO01l
037000030718     C                   IF        not %EQUAL
037100030723     c                   clear                   evoNTW
037200030723     c                   clear                   evoFIL
037300030722     c     kEVO          setll     tnEVO01l
037400030718     c                   end
037500030718     c                   endIF
037600030718      *
037700030718     C                   IF        %EQUAL
037800030718      *
037900030718     C                   DOU       %EOF
038000030718     C     kEVO          Reade     tnEVO01l
038100030718     C                   IF        NOT %EOF        and
038200040902     C                             evoDDA <= data_oggi and
038300040902     C                             evoADA >= data_oggi
038400030718     c                   move      evoGRP        Gruppo
038500030718     c                   leave
038600030718     C                   ENDIF
038700030718     C                   ENDDO
038800030627      *
038900030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
039000030718      *  legato alla tabella EVO.
039100030627     c                   z-add     1             Ic                3 0
039200030718     c     Gruppo        lookup    Grp(Ic)                                21
039300030627     c  n21              z-add     1             Ic
039400030630     c  n21*blank        lookup    Grp(Ic)                                21
039500030718     c                   movel     Gruppo        Grp(Ic)
039600030627     c                   add       Importo       ImC(Ic)
039700030721      *
039800030721     c                   end
039900030703      *
040000030627     c                   Endsr
040100030627      *----------------------------------------------------------------
040200050907     c     *inzsr        Begsr
040300050907      *
040400050907      *
040500050907     c                   Endsr
040600050907      *----------------------------------------------------------------
