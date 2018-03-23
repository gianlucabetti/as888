000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600160721     FTitas30C  iF   E           k DISK
000700030624     FTita430C  iF   E           k DISK
000800030627     FTita730C  iF   E           k DISK
000900050907     FAzOrg01L  iF   E           k DISK    usropn
001000030718     FEcCev30C  iF   E           k DISK
001100030718     FTneVA01L  iF   E           k DISK
001200030718     FTneVO01L  iF   E           k DISK
001300160721     FTNECF00F  uF   E             DISK    rename(TNECF000:TNECFIS)
001400160721     FTNECF01L  uF   E           k DISK    rename(TNECF000:TNECF1)
001500160721     FTNECF02L  uF   E           k DISK
001600030620      *
001700160721     D savAAo          S                   like(ecfAAS)
001800160721     D savLPo          S                   like(ecfLNP)
001900160721     D savNRo          S                   like(ecfNRS)
002000160721     D savNSo          S                   like(ecfNSP)
002100160721     D savRIF          S                   like(ecfRIF)
002200160721     D savGRP          S                   like(ecfGRP)
002300160721     D savNRREC        S              9s 0
002400160721      *
002500081022     D Scrivere        S              2
002600091001     D Importo         S             13  5
002700030630     D GRp             S              3    DIM(50)
002800091001     D IMr             S             16  5 DIM(50)
002900091001     D IMc             S             16  5 DIM(50)
003000091001     D IMp             S             16  5 DIM(50)
003100030718      *
003200030721      * Importo Voci spedizione (0)
003300030721    >D EcceV0        E DS                  extname(eccev00f)
003400030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
003500030721     D CodV0           S              3  0 DIM(50)
003600030721     D                                     BASED(Ptc0)
003700030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
003800090910     D ImpV0           S             13  5 DIM(50)
003900030721     D                                     BASED(Pti0)
004000030721      * Importo Voci spedizione (P)
004100030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
004200030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
004300030721     D CodVP           S              3  0 DIM(50)
004400030721     D                                     BASED(Ptcv)
004500030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
004600090910     D ImpVP           S             13  5 DIM(50)
004700030721     D                                     BASED(PtiV)
004800030627      *
004900030624     D KPJBA         E DS
005000030625     D OG143         E DS
005100070927     D DSbl4E        E DS
005200030625      *
005300030625     D WLBDAT          DS
005400030625     D  G02DAT                 1      8  0
005500030625     D  G02INV                 9     16  0
005600030625     D  G02ERR                17     17
005700030625     D  G02TGI                18     22  0
005800040902      *
005900040902     D  data_Oggi      s                   like(G02INV) INZ(0)
006000030624      *
006100030624     D param           DS
006200030624     D   kAas                  1      4  0
006300030624     D   kLnp                  5      7  0
006400030624     D   kNrs                  8      9  0
006500030624     D   kNsp                 10     16  0
006600060303      *
006700060303     D TAS_Bolla       DS
006800060303     D   sTASaas               1      4  0
006900060303     D   sTASLnp               5      7  0
007000060303     D   sTASNrs               8      9  0
007100060303     D   sTASNsp              10     16  0
007200060303     D   TASBolla14            3     16  0
007300030721      *
007400050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
007500050907      *
007600050907     D  Ofil           s                   like(orgfil) DIM(999)
007700050907     D  Ode3           s                   like(orgde3) DIM(999)
007800050907     d  og             s              5  0
007900030721      *---------------------------------------------------------------------*
008000030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
008100030721     IECCEV000      01
008200030721     IECCEVP00      02
008300030620      *---------------------------------------------------------------------*
008400030624     C     *entry        plist
008500160721     c                   parm                    savAAo
008600160721     c                   parm                    savLPo
008700160721     c                   parm                    savNRo
008800160721     c                   parm                    savNSo
008900160721     c                   parm                    savRIF
009000160721     c                   parm                    savGRP
009100160721     c                   parm                    savNRREC
009200160721      *
009300160721      *  imposta UDATE
009400160721     C                   move      *date         G02DAT
009500160721     C                   MOVE      *ZEROS        G02INV
009600160721     C                   MOVE      *BLANKS       G02ERR
009700160721     C                   CALL      'XSRDA8'
009800160721     C                   PARM                    WLBDAT
009900160721      *  è diventata la data consolidamento non più la data elaborazione
010000160721     C                   Z-ADD     G02INV        data_oggi
010100160721     C                   move      'DPD'         kDPD              3
010200030627      *
010300030718     c     kEVA          klist
010400030722     c                   kfld                    evaVAR
010500030722     c                   kfld                    evaNTW
010600030722     c                   kfld                    evaFIL
010700030718      *
010800030718     c     kEVO          klist
010900030722     c                   kfld                    evoNTW
011000030722     c                   kfld                    evoFIL
011100030721     c                   kfld                    evoVOC
011200030627      *
011300030627     c     ktas1         klist
011400030627     c                   kfld                    kAas
011500030624     c                   kfld                    kLnp
011600030624     c                   kfld                    kNrs
011700030624     c                   kfld                    kNsp
011800030625      *
011900030627     c     ktas3         klist
012000030625     c                   kfld                    tasAas
012100030625     c                   kfld                    tasLnp
012200030625     c                   kfld                    tasNrs
012300030625     c                   kfld                    tasNsp
012400030625     c                   kfld                    tasTbl
012500040830      *
012600040830     c     kECF          klist
012700040830     c                   kfld                    tasAas
012800040830     c                   kfld                    tasLnp
012900040830     c                   kfld                    tasNrs
013000040830     c                   kfld                    tasNsp
013100040902      *
013200040902     c     kECF1         klist
013300160721     c                   kfld                    kDPD
013400160721     c                   kfld                    savRIF
013500081022      *
013600160721      ***
013700160721     c                   eval      kAas =  savAAo
013800160721     c                   eval      kLnp =  savLPo
013900160721     c                   eval      kNrs =  savNRo
014000160721     c                   eval      kNsp =  savNSo
014100160721      ***
014200160721     c     ktas1         setll     titas30c
014300160721     c     ktas1         reade     titas30c
014400050907      ***
014500160721     c                   dow       not %Eof(titas30c)
014600030625      *
014700160721     c                   if        tasCCA = '2' or tasCCA = '6'
014800160721     c                   exsr      NetW
014900050907      *            * ------------------------------- *
015000160721     c                   exsr      campi_mamma
015100050907      *            * ------------------------------- *
015200160721     C                   End
015300050907      *
015400160721     c     ktas1         reade     titas30c
015500160721     C                   Enddo
015600050907      ***
015700160721     c                   seton                                        LR
015800030627      *----------------------------------------------------------------
015900030627      * imposta i campi per la scrittura
016000030627      *----------------------------------------------------------------
016100160721     c     campi_mamma   Begsr
016200040830      *
016300040830     c                   clear                   Grp
016400040830     c                   clear                   ImR
016500040830     c                   clear                   ImC
016600040830     c                   clear                   ImP
016700040902     **
016800040902     **  controlla che non esistessero precedenente dei records incompleti
016900040902     **   ossia non ancora fatturati da Bartolini e già addebitati dal Partner
017000040908     **   oppure non presenti nel sistema Bartolini e già addebitati dal Partner
017100040902     c                   exsr      chk_e_delete
017200160721      *
017300040902      *-------------------
017400030627      * Imposta i Ricavi
017500050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
017600050216     c                             tastbl <> 'AP'
017700030627     c                   exsr      Ricavi
017800050216     c                   end
017900050216      *
018000030627      * Imposta le Competenze
018100030627     c                   exsr      Competenze
018200160721      *
018300160721      *  si deve attribuire Ricavo e Competenza
018400160721     c     savNRREC      chain     TNECF00F
018500160721     c                   if        %Found(TNECF00F)
018600030627      *
018700030630     c                   z-add     1             Ir
018800160721     c     savGRP        lookup    Grp(Ir)                                21
018900160721      *
019000160721      *  somma ricavi e competenze
019100160721     c   21              add  (h)  ImR(Ir)       ECFIMR
019200160721     c   21              add  (h)  ImC(Ir)       ECFIMC
019300160721     c   21              add  (h)  ImP(Ir)       ECFIMP
019400160721      *
019500160721     c                   update    tnecfis
019600081022     c                   end
019700030630      *
019800030627     c                   Endsr
019900030627      *----------------------------------------------------------------
020000040902      *  Controlla e cancella records anomali precedentemente scritti
020100030627      *----------------------------------------------------------------
020200040902     c     chk_e_delete  Begsr
020300040902      *
020400040902     c                   clear                   savdta            8 0
020500040902      * Per prima cosa:
020600040902      *  Controlla che non fosse già presente un record non ancora fatturato
020700040902     c     kecf          setll     tnecf02l
020800040902     c     kecf          reade     tnecf02l
020900040902      *
021000040902     c                   dow       not %eof(tnecf02l)
021100040902      *
021200040902      *  Non deve essere Consolidato
021300160721      * se era stato scritto precedentemente
021400160721     c                   if        ecfdtg = *zero     and
021500160721     c                             ecftpr ='NFT'      and
021600160721     c                             ecfgrp =savGRP
021700040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
021800040902      *  legato alla tabella EVA.
021900040902     c                   z-add     1             Ir                3 0
022000040902     c     ecfGRP        lookup    Grp(Ir)                                21
022100040902     c  n21              z-add     1             Ir
022200040902     c  n21*blank        lookup    Grp(Ir)                                21
022300040902     c                   movel     ecfGRP        Grp(Ir)
022400040902     c                   add       ecfIMP        ImP(Ir)
022500040902     c                   delete    tnecf000
022600040902     c                   end
022700040902      *
022800040902     c     kecf          reade     tnecf02l
022900040902     c                   end
023000040902      *
023100160721      *  Controlla che fosse presente un record non trovato
023200040902     c     kecf1         setll     tnecf01l
023300040902     c     kecf1         reade     tnecf01l
023400040902      *
023500040902     c                   dow       not %eof(tnecf01l)
023600040902      *
023700040902      *  Non deve essere Consolidato
023800160721      * se era stato scritto precedentemente
023900160721     c                   if        ecfdtg = *zero   and
024000160721     c                             ecftpr ='NTR'    and
024100160721     c                             ecfgrp =savGRP
024200040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
024300040902      *  legato alla tabella EVA.
024400040902     c                   z-add     1             Ir                3 0
024500040902     c     ecfGRP        lookup    Grp(Ir)                                21
024600040902     c  n21              z-add     1             Ir
024700040902     c  n21*blank        lookup    Grp(Ir)                                21
024800040902     c                   movel     ecfGRP        Grp(Ir)
024900040902     c                   add       ecfIMP        ImP(Ir)
025000040902     c                   delete    tnecf1
025100040902     c                   end
025200040902      *
025300040902     c     kecf1         reade     tnecf01l
025400040902     c                   end
025500040902      *
025600040902     c                   Endsr
025700030627      *----------------------------------------------------------------
025800030627      *  imposta i Ricavi raggruppati su schiere
025900030627      *----------------------------------------------------------------
026000030627     c     Ricavi        Begsr
026100030627      *
026200030627      * IMPORTO Ricavo
026300030627      *  Porto
026400030627     c                   if        tasPOR > 0
026500030722     c                   clear                   evaVAR
026600030722     c                   movel     '1'           evaVAR
026700160126     c                   if        tasPOR  <> 8888888
026800030627     c                   z-add     tasPOR        importo
026900160126     c                   end
027000030718     c                   exsr      Tbe_EVA
027100030627     c                   end
027200030627      *  Varie 1
027300030627     c                   if        tasVA1 > 0
027400030722     c                   clear                   evaVAR
027500030722     c                   movel     tasSV1        evaVAR
027600160126     c                   if        tasVA1  <> 8888888
027700030627     c                   z-add     tasVA1        importo
027800160126     c                   end
027900030718     c                   exsr      Tbe_EVA
028000030627     c                   end
028100030627      *  Varie 2
028200030627     c                   if        tasVA2 > 0
028300030722     c                   clear                   evaVAR
028400030722     c                   movel     tasSV2        evaVAR
028500160126     c                   if        tasVA2  <> 8888888
028600030627     c                   z-add     tasVA2        importo
028700160126     c                   end
028800030718     c                   exsr      Tbe_EVA
028900030627     c                   end
029000030627      *  Varie 3
029100030627     c                   if        tasVA3 > 0
029200030722     c                   clear                   evaVAR
029300030722     c                   movel     tasSV3        evaVAR
029400160126     c                   if        tasVA3  <> 8888888
029500030627     c                   z-add     tasVA3        importo
029600160126     c                   end
029700030718     c                   exsr      Tbe_EVA
029800030627     c                   end
029900030627      *  Altre Varie
030000030627      *
030100030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
030200030627      *  e raggruppare.
030300030627     c     ktas3         setll     tita730c
030400030627     c     ktas3         reade     tita730c
030500030627     c                   dow       not %Eof(tita730c)
030600030722     c                   clear                   evaVAR
030700030722     c                   movel     ta7SVn        evaVAR
030800160126     c                   if        ta7VAn  <> 8888888
030900030627     c                   z-add     ta7VAn        importo
031000160126     c                   end
031100030718     c                   exsr      Tbe_EVA
031200030627     c     ktas3         reade     tita730c
031300030627     c                   enddo
031400030627      *
031500030627     c                   Endsr
031600030627      *----------------------------------------------------------------
031700030627      *  Decodifica il raggruppamento per le Varie
031800030627      *----------------------------------------------------------------
031900030718     c     TBE_EVA       Begsr
032000030627      *
032100030722     c                   clear                   evaNTW
032200030722     c                   clear                   evaFIL
032300030718     c                   move      '999'         Gruppo            3
032400030721      *
032500030721      * prova se ci sono delle particolarità sul Partner + LNP/A
032600030722      *  chiave completa
032700160721     c                   movel     kDPD          evaNTW
032800030722     c                   move      Linea_Cod     evaFIL
032900030721     c     kEVA          setll     tnEVA01l
033000030721     C                   IF        not %EQUAL
033100030721      * se ci sono dei codici particolari da prendere in considerazione
033200030627      *  al posto di quelli standard.
033300030722      *  chiave parziale
033400160721     c                   movel     kDPD          evaNTW
033500030722     c                   clear                   evaFIL
033600030718     c     kEVA          setll     tnEVA01l
033700030718     C                   IF        not %EQUAL
033800030722     c                   clear                   evaNTW
033900030722     c                   clear                   evaFIL
034000030722     c     kEVA          setll     tnEVA01l
034100030718     c                   end
034200030721     c                   endIF
034300030627      *
034400030718     C                   IF        %EQUAL
034500030718      *
034600030718     C                   DOU       %EOF
034700030718     C     kEVA          Reade     tnEVA01l
034800030718     C                   IF        NOT %EOF and
034900040902     C                             evaDDA <= data_oggi and
035000040902     C                             evaADA >= data_oggi
035100030718     c                   move      evaGRP        Gruppo
035200030718     c                   leave
035300030718     C                   ENDIF
035400030718     C                   ENDDO
035500030718      *
035600030718     c                   end
035700030718      *
035800030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
035900030718      *  legato alla tabella EVA.
036000030627     c                   z-add     1             Ir                3 0
036100030718     c     Gruppo        lookup    Grp(Ir)                                21
036200030627     c  n21              z-add     1             Ir
036300030630     c  n21*blank        lookup    Grp(Ir)                                21
036400030718     c                   movel     Gruppo        Grp(Ir)
036500030627     c                   add       Importo       ImR(Ir)
036600030627      *
036700030627     c                   Endsr
036800030627      *----------------------------------------------------------------
036900030627      *  imposta le Competenze raggruppate su schiere
037000030627      *----------------------------------------------------------------
037100030627     c     Competenze    Begsr
037200030627      *
037300030721     c                   clear                   ImpV0
037400030721     c                   clear                   ImpVP
037500030721     c                   clear                   CodV0
037600030721     c                   clear                   CodVP
037700030721      *
037800030627      * IMPORTO Competenze
037900030718     c     ktas3         chain     Eccev30C
038000030718     c                   if        %Found(Eccev30C)
038100030627      *
038200030721     C                   Do        50            CV                3 0
038300030721      *
038400030721     C                   If        *in01 = *on
038500030721      * esce quando non ha più Voci in skiera
038600030721     C                   If        CodV0(CV) = 0
038700030721     c                   leave
038800030721     c                   end
038900030721     c                   clear                   evoVOC
039000030721     c                   movel     CodV0(CV)     evoVOC
039100030721     c                   z-add     ImpV0(CV)     importo
039200030721     c                   exsr      Tbe_EVO
039300030721     c                   end
039400030718      *
039500030721     C                   If        *in02 = *on
039600030721      * esce quando non ha più Voci in skiera
039700030721     C                   If        CodVP(CV) = 0
039800030721     c                   leave
039900030721     c                   end
040000030721     c                   clear                   evoVOC
040100030721     c                   movel     CodVP(CV)     evoVOC
040200030721     c                   z-add     ImpVP(CV)     importo
040300030721     c                   exsr      Tbe_EVO
040400030721     c                   end
040500030721      *
040600030721     c                   endDo
040700030627      *
040800030721     C                   endIF
040900030721      *
041000030627     c                   Endsr
041100030627      *----------------------------------------------------------------
041200030627      *  Decodifica il raggruppamento per le Voci
041300030627      *----------------------------------------------------------------
041400030718     c     TBE_EVO       Begsr
041500030722      *
041600030722     c                   clear                   evoNTW
041700030722     c                   clear                   evoFIL
041800030722     c                   move      '999'         Gruppo            3
041900030722      *
042000030722      * prova se ci sono delle particolarità sul Partner + LNP/A
042100030722      *  chiave completa
042200160721     c                   movel     kDPD          evoNTW
042300030722     c                   move      Linea_Cod     evoFIL
042400030718     c     kEVO          setll     tnEVO01l
042500030718     C                   IF        not %EQUAL
042600030722      * se ci sono dei codici particolari da prendere in considerazione
042700030722      *  al posto di quelli standard.
042800030722      *  chiave parziale
042900160721     c                   movel     kDPD          evoNTW
043000030723     c                   clear                   evoFIL
043100030718     c     kEVO          setll     tnEVO01l
043200030718     C                   IF        not %EQUAL
043300030723     c                   clear                   evoNTW
043400030723     c                   clear                   evoFIL
043500030722     c     kEVO          setll     tnEVO01l
043600030718     c                   end
043700030718     c                   endIF
043800030718      *
043900030718     C                   IF        %EQUAL
044000030718      *
044100030718     C                   DOU       %EOF
044200030718     C     kEVO          Reade     tnEVO01l
044300030718     C                   IF        NOT %EOF        and
044400040902     C                             evoDDA <= data_oggi and
044500040902     C                             evoADA >= data_oggi
044600030718     c                   move      evoGRP        Gruppo
044700030718     c                   leave
044800030718     C                   ENDIF
044900030718     C                   ENDDO
045000030627      *
045100030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
045200030718      *  legato alla tabella EVO.
045300030627     c                   z-add     1             Ic                3 0
045400030718     c     Gruppo        lookup    Grp(Ic)                                21
045500030627     c  n21              z-add     1             Ic
045600030630     c  n21*blank        lookup    Grp(Ic)                                21
045700030718     c                   movel     Gruppo        Grp(Ic)
045800030627     c                   add       Importo       ImC(Ic)
045900030721      *
046000030721     c                   end
046100030703      *
046200030627     c                   Endsr
046300030627      *----------------------------------------------------------------
046400030627      *  Decodifica il Network
046500030627      *----------------------------------------------------------------
046600030627     c     Netw          Begsr
046700030627      *
046800030828     c                   clear                   ImpExp            1
046900030627      *
047000030627      *  x LNP controlla Estero
047100030627     C                   clear                   OG143
047200050907     c                   z-add     1             og
047300160721     c     tasLNP        lookup    Ofil(og)                               99
047400080404     c                   if        %Equal
047500050907     C                   movel     Ode3(og)      OG143
047600030627     c                   end
047700030627      *
047800030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
047900030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
048000030627      *  se non fosse risultato tale allora deve controllare x LNA.
048100160721     c                   if         §ogNTW = 'DPD'
048200030828     c                   movel     'I'           ImpExp
048300030721     c                   movel     'LNP'         Estero_LN         3
048400030721     c                   z-add     tasLNP        Linea_Cod         3 0
048500030627     c                   else
048600030627     C                   clear                   OG143
048700030703      * prova per LNA -> se prima, x LNP il network non è un estero
048800050907     c                   z-add     1             og
048900050907     c     tasLNA        lookup    Ofil(og)                               99
049000080404     c                   if        %Equal
049100050907     C                   movel     Ode3(og)      OG143
049200030828     c                   movel     'E'           ImpExp
049300030721     c                   movel     'LNA'         Estero_LN
049400030721     c                   z-add     tasLNA        Linea_Cod
049500030627     c                   end
049600030627     c                   end
049700030627      *
049800030627     c                   Endsr
049900030627      *----------------------------------------------------------------
050000050907     c     *inzsr        Begsr
050100050907      *
050200050907      *  carica AZORG una volta per tutte:
050300050907     c                   clear                   oFil
050400050907     c                   clear                   oDe3
050500050907     c                   open      AzOrg01L
050600050907     c     *loval        setll     AzOrg01L
050700050907     c                   read      AzOrg01L
050800050907     c                   DOW       not %EoF(AzOrg01L)
050900050907     c                   add       1             og
051000050907     c                   move      orgfil        oFIL(og)
051100050907     c                   move      orgde3        oDE3(og)
051200050907     c                   read      AzOrg01L
051300050907     c                   enddo
051400050907     c                   close     AzOrg01L
051500050907      *
051600050907     c                   Endsr
051700050907      *----------------------------------------------------------------
