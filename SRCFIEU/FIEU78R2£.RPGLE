000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600050907     F***Titas30C  iF   E           k DISK
000700030624     FTita430C  iF   E           k DISK
000800030627     FTita730C  iF   E           k DISK
000900050907     FAzOrg01L  iF   E           k DISK    usropn
001000030718     FEcCev30C  iF   E           k DISK
001100030627     FTnTbe01L  iF   E           k DISK
001200030718     FTneVA01L  iF   E           k DISK
001300030718     FTneVO01L  iF   E           k DISK
001400080617     FFnLBL01L  iF   E           k DISK
001500040830     FTNECF02L  uF a E           k DISK
001600040902     FTNECF01L  uF   E           k DISK    rename(TNECF000:TNECF1)
001700030620      *
001800030627     D Importo         S             13  3
001900030630     D GRo             S              3    DIM(50) descend
002000030630     D GRp             S              3    DIM(50)
002100030627     D IMr             S             16  3 DIM(50)
002200030627     D IMc             S             16  3 DIM(50)
002300040830     D IMp             S             16  3 DIM(50)
002400040830     D Dfp             S              8  0 DIM(50)
002500040830     D Nfp             S             15    DIM(50)
002600030718      *
002700030721      * Importo Voci spedizione (0)
002800030721    >D EcceV0        E DS                  extname(eccev00f)
002900030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
003000030721     D CodV0           S              3  0 DIM(50)
003100030721     D                                     BASED(Ptc0)
003200030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
003300030721     D ImpV0           S              7  3 DIM(50)
003400030721     D                                     BASED(Pti0)
003500030721      * Importo Voci spedizione (P)
003600030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
003700030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003800030721     D CodVP           S              3  0 DIM(50)
003900030721     D                                     BASED(Ptcv)
004000030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
004100030721     D ImpVP           S              7  3 DIM(50)
004200030721     D                                     BASED(PtiV)
004300030627      *
004400030624     D KPJBA         E DS
004500030625     D OG143         E DS
004600070927     D DSbl4E        E DS
004700030625      *
004800030625     D WLBDAT          DS
004900030625     D  G02DAT                 1      8  0
005000030625     D  G02INV                 9     16  0
005100030625     D  G02ERR                17     17
005200030625     D  G02TGI                18     22  0
005300040902      *
005400040902     D  data_Oggi      s                   like(G02INV) INZ(0)
005500030624      *
005600030624     D param           DS
005700030624     D   kAas                  1      4  0
005800030624     D   kLnp                  5      7  0
005900030624     D   kNrs                  8      9  0
006000030624     D   kNsp                 10     16  0
006100030624     D   kTbl                 17     18
006200030703     D   kNtw                 19     21
006300060303      *
006400060303     D TAS_Bolla       DS
006500060303     D   sTASaas               1      4  0
006600060303     D   sTASLnp               5      7  0
006700060303     D   sTASNrs               8      9  0
006800060303     D   sTASNsp              10     16  0
006900060303     D   TASBolla14            3     16  0
007000030721      *
007100050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
007200050907      *
007300050907     D  Ofil           s                   like(orgfil) DIM(999)
007400050907     D  Ode3           s                   like(orgde3) DIM(999)
007500050907     d  og             s              5  0
007600030721      *---------------------------------------------------------------------*
007700030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
007800030721     IECCEV000      01
007900030721     IECCEVP00      02
008000030620      *---------------------------------------------------------------------*
008100030624     C     *entry        plist
008200030624     C                   parm                    kpjba
008300050907     C                   parm                    DsTAS
008400050907      **** la DsTAS sostituisce la lettura dello stesso record del TITAS
008500050907      **** avendo il pgm già selezionato il record da elaborare nel FIEU78R.
008600030624     c                   movel     kpjbu         param
008700030627      *
008800030627     c     ktbe          klist
008900030627     c                   kfld                    tbeCOD
009000030627     c                   kfld                    tbeKE1
009100030627     c                   kfld                    tbeKE2
009200030718      *
009300030718     c     kEVA          klist
009400030722     c                   kfld                    evaVAR
009500030722     c                   kfld                    evaNTW
009600030722     c                   kfld                    evaFIL
009700030718      *
009800030718     c     kEVO          klist
009900030722     c                   kfld                    evoNTW
010000030722     c                   kfld                    evoFIL
010100030721     c                   kfld                    evoVOC
010200030627      *
010300030627     c     ktas1         klist
010400030627     c                   kfld                    kAas
010500030624     c                   kfld                    kLnp
010600030624     c                   kfld                    kNrs
010700030624     c                   kfld                    kNsp
010800030624      *
010900030627     c     ktas2         klist
011000030624     c                   kfld                    kAas
011100030624     c                   kfld                    kLnp
011200030624     c                   kfld                    kNrs
011300030624     c                   kfld                    kNsp
011400030624     c                   kfld                    kTbl
011500030625      *
011600030627     c     ktas3         klist
011700030625     c                   kfld                    tasAas
011800030625     c                   kfld                    tasLnp
011900030625     c                   kfld                    tasNrs
012000030625     c                   kfld                    tasNsp
012100030625     c                   kfld                    tasTbl
012200080617      *
012300080617     c     klbl1         klist
012400080617     c                   kfld                    tasAas
012500080617     c                   kfld                    tasLnp
012600080617     c                   kfld                    tasNrs
012700080617     c                   kfld                    tasNsp
012800040830      *
012900080617     c     klbl1_1       klist
013000080617     c                   kfld                    lblAAP
013100080617     c                   kfld                    lblLPP
013200080617     c                   kfld                    lblNRP
013300080617     c                   kfld                    lblNSP
013400080617      *
013500040830     c     kECF          klist
013600040830     c                   kfld                    tasAas
013700040830     c                   kfld                    tasLnp
013800040830     c                   kfld                    tasNrs
013900040830     c                   kfld                    tasNsp
014000040902      *
014100040902     c     kECF1         klist
014200040902     c                   kfld                    ecfntw
014300040902     c                   kfld                    ecfrif
014400050907      ***
014500050907     c***                if        kTbl = *blank
014600050907     c***  ktas1         setll     titas30c
014700050907     c***  ktas1         reade     titas30c
014800050907     c***                else
014900050907     c***  ktas2         setll     titas30c
015000050907     c***  ktas2         reade     titas30c
015100050907     c***                end
015200050907      ***
015300050907     c***                dow       not %Eof(titas30c)
015400030625      *
015500050907      *            * ------------------------------- *
015600030627     c                   exsr      campi_ECF
015700050907      *            * ------------------------------- *
015800050907      *
015900050907      ***
016000050907     c***                if        kTbl = *blank
016100050907     c***  ktas1         reade     titas30c
016200050907     c***                else
016300050907     c***  ktas2         reade     titas30c
016400050907     c***                end
016500050907      ***
016600050907     C***                Enddo
016700050907      ***
016800050907     c***                seton                                        LR
016900050907     c                   seton                                        RT
017000030627      *----------------------------------------------------------------
017100030627      * imposta i campi per la scrittura
017200030627      *----------------------------------------------------------------
017300030627     c     campi_ECF     Begsr
017400040830      *
017500040830     c                   clear                   Gro
017600040830     c                   clear                   Grp
017700040830     c                   clear                   ImR
017800040830     c                   clear                   ImC
017900040830     c                   clear                   ImP
018000040830     c                   clear                   DfP
018100040830     c                   clear                   NfP
018200040902     **
018300040902     **  controlla che non esistessero precedenente dei records incompleti
018400040902     **   ossia non ancora fatturati da Bartolini e già addebitati dal Partner
018500040908     **   oppure non presenti nel sistema Bartolini e già addebitati dal Partner
018600040902     c                   exsr      chk_e_delete
018700040902     **
018800040902      *-------------------
018900040830     c                   clear                   tnecf000
019000040116      *
019100040116      * Decodifica il Network
019200040116     c                   exsr      NetW
019300040909      *
019400040909      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
019500040909      *   per poter distinguere di quale Partner si tratta
019600060303     c                   clear                   savNTW            3 0
019700040909     c                   if         ecfNTW = 'EEX'
019800040909     c                   move      Linea_Cod     ecfNTW
019900060303     c                   move      Linea_Cod     savNTW            3 0
020000040909     c                   end
020100040909      *
020200040116     c                   if        KNtw <> *blank and
020300040116     c                             Kntw <> ecfNTW
020400040116     c                   goto      No_write
020500040116     c                   End
020600030627      *  imposta UDATE
020700030627     C                   move      *date         G02DAT
020800030627     C                   MOVE      *ZEROS        G02INV
020900030627     C                   MOVE      *BLANKS       G02ERR
021000030627     C                   CALL      'XSRDA8'
021100030627     C                   PARM                    WLBDAT
021200040902      *  è diventata la data consolidamento non più la data elaborazione
021300040902     C                   Z-ADD     G02INV        data_oggi
021400040902     C*********          Z-ADD     G02INV        ECFDTG
021500030627      *
021600030627     c                   movel     tasAAS        ECFAAS
021700030627     c                   movel     tasLNP        ECFLNP
021800030627     c                   movel     tasNRS        ECFNRS
021900030627     c                   movel     tasNSP        ECFNSP
022000030627     c                   movel     tasLNA        ECFLNA
022100030627     c                   movel     tasMGS        ECFMGS
022200030627     c                   movel     tasTBL        ECFTBL
022300030627     c                   movel     tasFBR        ECFFBR
022400080617      *
022500080617      * se si tratta di una bolla di reso cerca la bolla originale
022600080617     c                   if        tasFBR = 'S'
022700080617     c                   exsr      Cerca_la_mamma
022800080617     c                   end
022900080617      *
023000030627     c                   movel     tasKSC        ECFKSC
023100030627     c                   movel     tasDTI        ECFDTI
023200030627     c                   movel     tasDCM        ECFDCM
023300030627     c                   movel     tasCCA        ECFCCA
023400030627     c                   movel     tasNCL        ECFNCL
023500030627     c                   movel     tasPKF        ECFPKF
023600030627     c                   movel     tasVLF        ECFVLF
023700040902     c                   z-add     tasRMN        ECFRMN
023800030703      *
023900030703      * dta e nr.fattura del Partner
024000030707     c                   movel     tasNFT        ECFNFT
024100030707     c                   movel     tasDFT        ECFDFT
024200030627      *
024300030627     c                   exsr      Rif_Partner
024400030630      *
024500040902     c                   if        savdta = 0
024600040830     c                   movel     *zeros        ECFDTA
024700040902     c                   else
024800040902     C                   Z-ADD     data_oggi     ecfdta
024900040909     C                   move      '28'          ecfdta
025000040902     c                   end
025100040902      *
025200030627      * Imposta i Ricavi
025300050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
025400050216     c                             tastbl <> 'AP'
025500030627     c                   exsr      Ricavi
025600050216     c                   end
025700050216      *
025800030627      * Imposta le Competenze
025900030627     c                   exsr      Competenze
026000030627      *
026100030630      * riordina i dati immagazzinati per scrivere i records
026200030630     c                   movea     grp           gro
026300030630     c                   sorta     gro
026400030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
026500030630      *    nelle schiere
026600030630     c                   do        50            cnt               3 0
026700041015      *
026800041015     c                   select
026900041015      *
027000041015     c                   when      gro(1) = *blank and cnt = 1
027100041015     c                   move      '001'         gro(1)
027200041015      *
027300041015     c                   when      gro(cnt) = *blank and cnt > 1
027400030630     c                   leave
027500041015      *
027600041015     c                   endsl
027700030627      *
027800030723     c                   movel     gro(cnt)      ECFGRP
027900030630      *
028000030630     c                   z-add     1             Ir
028100030630     c     gro(cnt)      lookup    Grp(Ir)                                21
028200030630      * Ricavi/Competenze
028300041015     c   21              z-add     ImR(Ir)       ECFIMR
028400041015     c   21              z-add     ImC(Ir)       ECFIMC
028500040830      *
028600041015     c   21              z-add     ImP(Ir)       ECFIMP
028700041015     c   21              z-add     DfP(Ir)       ECFDFP
028800041015     c   21              movel     NfP(Ir)       ECFNFP
028900030627      *
029000040830     c                   write     tnecf000
029100030630      *
029200030630     c                   EndDO
029300030703     c     No_Write      tag
029400030627      *
029500030627     c                   Endsr
029600030627      *----------------------------------------------------------------
029700040902      *  Controlla e cancella records anomali precedentemente scritti
029800030627      *----------------------------------------------------------------
029900040902     c     chk_e_delete  Begsr
030000040902      *
030100040902     c                   clear                   savdta            8 0
030200030829      *
030300040902      * Per prima cosa:
030400040902      *  Controlla che non fosse già presente un record non ancora fatturato
030500040902     c     kecf          setll     tnecf02l
030600040902     c     kecf          reade     tnecf02l
030700040902      *
030800040902     c                   dow       not %eof(tnecf02l)
030900040902      *
031000040902      *  Non deve essere Consolidato
031100040902     c                   if        ecfdtg = *zero
031200040902      * se era stato scritto precedentemente
031300040902     c                   if        ecftpr ='NFT'
031400040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
031500040902      *  legato alla tabella EVA.
031600040902     c                   z-add     1             Ir                3 0
031700040902     c     ecfGRP        lookup    Grp(Ir)                                21
031800040902     c  n21              z-add     1             Ir
031900040902     c  n21*blank        lookup    Grp(Ir)                                21
032000040902     c                   movel     ecfGRP        Grp(Ir)
032100040902     c                   add       ecfIMP        ImP(Ir)
032200040902     c                   movel     ecfNFP        NfP(Ir)
032300040902     c                   z-add     ecfDFP        DfP(Ir)
032400040908     c                   z-add     data_oggi     savdta
032500040902     c                   delete    tnecf000
032600040902     c                   end
032700040902     c                   end
032800040902      *
032900040902     c     kecf          reade     tnecf02l
033000040902     c                   end
033100040902      *
033200040902      * Per seconda cosa:
033300040902      *  Controlla che fosse presente un record non trovato
033400040902      *
033500040902      * Decodifica il Network
033600040902     c                   exsr      NetW
033700040902      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
033800040902      *   per poter distinguere di quale Partner si tratta
033900040902     c                   if         ecfNTW = 'EEX'
034000040902     c                   move      Linea_Cod     ecfNTW
034100040902     c                   end
034200040909      *
034300040902     c                   exsr      Rif_Partner
034400040902      *
034500040902     c     kecf1         setll     tnecf01l
034600040902     c     kecf1         reade     tnecf01l
034700040902      *
034800040902     c                   dow       not %eof(tnecf01l)
034900040902      *
035000040902      *  Non deve essere Consolidato
035100040902     c                   if        ecfdtg = *zero
035200040902      * se era stato scritto precedentemente
035300040902     c                   if        ecftpr ='NTR'
035400040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
035500040902      *  legato alla tabella EVA.
035600040902     c                   z-add     1             Ir                3 0
035700040902     c     ecfGRP        lookup    Grp(Ir)                                21
035800040902     c  n21              z-add     1             Ir
035900040902     c  n21*blank        lookup    Grp(Ir)                                21
036000040902     c                   movel     ecfGRP        Grp(Ir)
036100040902     c                   add       ecfIMP        ImP(Ir)
036200040902     c                   movel     ecfNFP        NfP(Ir)
036300040902     c                   z-add     ecfDFP        DfP(Ir)
036400040908     c                   z-add     data_oggi     savdta
036500040902     c                   delete    tnecf1
036600040902     c                   end
036700040902     c                   end
036800040902      *
036900040902     c     kecf1         reade     tnecf01l
037000040902     c                   end
037100040902      *
037200040902     c                   Endsr
037300040902      *----------------------------------------------------------------
037400040902      *  Riferimento Partner Estero solo se una Partenza
037500040902      *----------------------------------------------------------------
037600040902     c     Rif_Partner   Begsr
037700040902      *
037800030627      *  RIFERIMENTO Mittente  da tita4
037900030627     c     ktas1         setll     tita430c
038000030627     c     ktas1         reade     tita430c
038100030627      *
038200030627     c                   dow       not %Eof(tita430c)
038300030627      *
038400030829      * Se è un DPD x LNP/Network
038500030828     c                   if        §ogNTW = 'DPD'
038600030828      *
038700070619      *  x DPD deve prendere da TITA4 il record 'I'
038800070619     c                   if        ta4TRC = 'I'
038900080414     c                   eval      ECFRIF =  %subst(ta4not:1:14)
039000080414      *
039100080414      *
039200080414      * NON VA + BENE si salta la compattazione:
039300080414     c                   goto      NOcompatta
039400080414      *
039500070619     c                   if        %subst(ta4not:1:1) = '0' and
039600070619     c                             %subst(ta4not:5:2) = '99'
039700070619     c                   eval      fld11 = %subst(ta4not:2:3) +
039800070619     c                                     %subst(ta4not:7:8)
039900070619     c                   else
040000060626     c                   eval      ECFRIF =  %subst(ta4not:1:14)
040100030627     c                   end
040200080414      *
040300080414     c     NOcompatta    tag
040400080414      *
040500070619     c                   end
040600030828      *
040700030828     c                   else
040800030828      *
040900030828      * Se è un Fedex x LNP/Network
041000070927     c*****              if        ta4TRC = 'A'
041100070927     c                   if        ta4TRC = 'E'
041200060303      *
041300070927     c****               eval      ECFRIF = %subst(ta4not:16:20)
041400070927     c                   movel     ta4not        dsbl4E
041500070927     c                   movel     §b4ERP        ecfRIF
041600060303      **
041700060303      * ?Attenzione se si tratta di EUROEXPRESS Export il riferimento
041800060303      * ?  è la nostra bolla   NON DPD o FED --> ossia savNTW diverso da 0
041900060303     c                   if        savNTW <> 0
042000060303      * ? x Export il NTW deve essere uguale alla linea Arrivo
042100060303     c                   if        savNTW = ecfLNA or ecfRIF = *blank
042200060303     c                   eval      stasaas = tasaas
042300060303     c                   eval      staslnp = taslnp
042400060303     c                   eval      stasnrs = tasnrs
042500060303     c                   eval      stasnsp = tasnsp
042600060303     c                   movel     tasBolla14    ecfRIF
042700060303     c                   end
042800060303      *
042900060303     c                   end
043000060303      *
043100030828      *  se non c'è il riferimento allora imposta il rif.numerico
043200030828     c                   if        ecfRIF = *blank
043300040902     c                   movel     tasRMN        ecfRIF
043400030828     c                   end
043500060303      *
043600030828     c                   end
043700030828      *
043800030828     c                   end
043900030627      *
044000030627     c     ktas1         reade     tita430c
044100030627     c                   enddo
044200030828      *
044300030828      *  se Non trova un riferimento prende il rif_numerico
044400030828     c                   if        ecfRIF = *blank
044500060303      *
044600030828     c                   if        §ogNTW = 'DPD'
044700070619      *
044800070619     c                   select
044900070619      *
045000070619     c                   when      tasRMN > 100000000000 and
045100070619     c                             tasRMN < 999999999999
045200070619     c                   move      tasRMN        fld12            12
045300070619     c                   movel     fld12         fld11            11
045400070619     c                   movel(p)  fld11         ecfRIF
045500070619      *
045600070619     c                   when      tasRMN > 999999999999  and
045700070619     c                             tasRMN < 99999999999999
045800070619     c                   move      tasRMN        fld14            14
045900070619     c                   movel(p)  fld14         ecfRIF
046000070619      *
046100070619     c                   endSL
046200070619      *
046300030828     c                   else
046400060303      **
046500060303      * ?Imposta come riferimento la nostra Bolla
046600060303     c                   eval      stasaas = tasaas
046700060303     c                   eval      staslnp = taslnp
046800060303     c                   eval      stasnrs = tasnrs
046900060303     c                   eval      stasnsp = tasnsp
047000060303     c                   movel     tasBolla14    ecfRIF
047100060303      **
047200030828     c                   end
047300030828     c                   end
047400030828      *
047500030829      *
047600030627     c                   Endsr
047700030627      *----------------------------------------------------------------
047800030627      *  imposta i Ricavi raggruppati su schiere
047900030627      *----------------------------------------------------------------
048000030627     c     Ricavi        Begsr
048100030627      *
048200030627      * IMPORTO Ricavo
048300030627      *  Porto
048400030627     c                   if        tasPOR > 0
048500030722     c                   clear                   evaVAR
048600030722     c                   movel     '1'           evaVAR
048700030627     c                   z-add     tasPOR        importo
048800030718     c                   exsr      Tbe_EVA
048900030627     c                   end
049000030627      *  Varie 1
049100030627     c                   if        tasVA1 > 0
049200030722     c                   clear                   evaVAR
049300030722     c                   movel     tasSV1        evaVAR
049400030627     c                   z-add     tasVA1        importo
049500030718     c                   exsr      Tbe_EVA
049600030627     c                   end
049700030627      *  Varie 2
049800030627     c                   if        tasVA2 > 0
049900030722     c                   clear                   evaVAR
050000030722     c                   movel     tasSV2        evaVAR
050100030627     c                   z-add     tasVA2        importo
050200030718     c                   exsr      Tbe_EVA
050300030627     c                   end
050400030627      *  Varie 3
050500030627     c                   if        tasVA3 > 0
050600030722     c                   clear                   evaVAR
050700030722     c                   movel     tasSV3        evaVAR
050800030627     c                   z-add     tasVA3        importo
050900030718     c                   exsr      Tbe_EVA
051000030627     c                   end
051100030627      *  Altre Varie
051200030627      *
051300030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
051400030627      *  e raggruppare.
051500030627     c     ktas3         setll     tita730c
051600030627     c     ktas3         reade     tita730c
051700030627     c                   dow       not %Eof(tita730c)
051800030722     c                   clear                   evaVAR
051900030722     c                   movel     ta7SVn        evaVAR
052000030627     c                   z-add     ta7VAn        importo
052100030718     c                   exsr      Tbe_EVA
052200030627     c     ktas3         reade     tita730c
052300030627     c                   enddo
052400030627      *
052500030627     c                   Endsr
052600030627      *----------------------------------------------------------------
052700030627      *  Decodifica il raggruppamento per le Varie
052800030627      *----------------------------------------------------------------
052900030718     c     TBE_EVA       Begsr
053000030627      *
053100030722     c                   clear                   evaNTW
053200030722     c                   clear                   evaFIL
053300030718     c                   move      '999'         Gruppo            3
053400030721      *
053500030721      * prova se ci sono delle particolarità sul Partner + LNP/A
053600030722      *  chiave completa
053700030722     c                   movel     §ogNtw        evaNTW
053800030722     c                   move      Linea_Cod     evaFIL
053900030721     c     kEVA          setll     tnEVA01l
054000030721     C                   IF        not %EQUAL
054100030721      * se ci sono dei codici particolari da prendere in considerazione
054200030627      *  al posto di quelli standard.
054300030722      *  chiave parziale
054400030722     c                   movel     §ogNtw        evaNTW
054500030722     c                   clear                   evaFIL
054600030718     c     kEVA          setll     tnEVA01l
054700030718     C                   IF        not %EQUAL
054800030722     c                   clear                   evaNTW
054900030722     c                   clear                   evaFIL
055000030722     c     kEVA          setll     tnEVA01l
055100030718     c                   end
055200030721     c                   endIF
055300030627      *
055400030718     C                   IF        %EQUAL
055500030718      *
055600030718     C                   DOU       %EOF
055700030718     C     kEVA          Reade     tnEVA01l
055800030718     C                   IF        NOT %EOF and
055900040902     C                             evaDDA <= data_oggi and
056000040902     C                             evaADA >= data_oggi
056100030718     c                   move      evaGRP        Gruppo
056200030718     c                   leave
056300030718     C                   ENDIF
056400030718     C                   ENDDO
056500030718      *
056600030718     c                   end
056700030718      *
056800030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
056900030718      *  legato alla tabella EVA.
057000030627     c                   z-add     1             Ir                3 0
057100030718     c     Gruppo        lookup    Grp(Ir)                                21
057200030627     c  n21              z-add     1             Ir
057300030630     c  n21*blank        lookup    Grp(Ir)                                21
057400030718     c                   movel     Gruppo        Grp(Ir)
057500030627     c                   add       Importo       ImR(Ir)
057600030627      *
057700030627     c                   Endsr
057800030627      *----------------------------------------------------------------
057900030627      *  imposta le Competenze raggruppate su schiere
058000030627      *----------------------------------------------------------------
058100030627     c     Competenze    Begsr
058200030627      *
058300030721     c                   clear                   ImpV0
058400030721     c                   clear                   ImpVP
058500030721     c                   clear                   CodV0
058600030721     c                   clear                   CodVP
058700030721      *
058800030627      * IMPORTO Competenze
058900030718     c     ktas3         chain     Eccev30C
059000030718     c                   if        %Found(Eccev30C)
059100030627      *
059200030721     C                   Do        50            CV                3 0
059300030721      *
059400030721     C                   If        *in01 = *on
059500030721      * esce quando non ha più Voci in skiera
059600030721     C                   If        CodV0(CV) = 0
059700030721     c                   leave
059800030721     c                   end
059900030721     c                   clear                   evoVOC
060000030721     c                   movel     CodV0(CV)     evoVOC
060100030721     c                   z-add     ImpV0(CV)     importo
060200030721     c                   exsr      Tbe_EVO
060300030721     c                   end
060400030718      *
060500030721     C                   If        *in02 = *on
060600030721      * esce quando non ha più Voci in skiera
060700030721     C                   If        CodVP(CV) = 0
060800030721     c                   leave
060900030721     c                   end
061000030721     c                   clear                   evoVOC
061100030721     c                   movel     CodVP(CV)     evoVOC
061200030721     c                   z-add     ImpVP(CV)     importo
061300030721     c                   exsr      Tbe_EVO
061400030721     c                   end
061500030721      *
061600030721     c                   endDo
061700030627      *
061800030721     C                   endIF
061900030721      *
062000030627     c                   Endsr
062100030627      *----------------------------------------------------------------
062200030627      *  Decodifica il raggruppamento per le Voci
062300030627      *----------------------------------------------------------------
062400030718     c     TBE_EVO       Begsr
062500030722      *
062600030722     c                   clear                   evoNTW
062700030722     c                   clear                   evoFIL
062800030722     c                   move      '999'         Gruppo            3
062900030722      *
063000030722      * prova se ci sono delle particolarità sul Partner + LNP/A
063100030722      *  chiave completa
063200030722     c                   movel     §ogNtw        evoNTW
063300030722     c                   move      Linea_Cod     evoFIL
063400030718     c     kEVO          setll     tnEVO01l
063500030718     C                   IF        not %EQUAL
063600030722      * se ci sono dei codici particolari da prendere in considerazione
063700030722      *  al posto di quelli standard.
063800030722      *  chiave parziale
063900030723     c                   movel     §ogNtw        evoNTW
064000030723     c                   clear                   evoFIL
064100030718     c     kEVO          setll     tnEVO01l
064200030718     C                   IF        not %EQUAL
064300030723     c                   clear                   evoNTW
064400030723     c                   clear                   evoFIL
064500030722     c     kEVO          setll     tnEVO01l
064600030718     c                   end
064700030718     c                   endIF
064800030718      *
064900030718     C                   IF        %EQUAL
065000030718      *
065100030718     C                   DOU       %EOF
065200030718     C     kEVO          Reade     tnEVO01l
065300030718     C                   IF        NOT %EOF        and
065400040902     C                             evoDDA <= data_oggi and
065500040902     C                             evoADA >= data_oggi
065600030718     c                   move      evoGRP        Gruppo
065700030718     c                   leave
065800030718     C                   ENDIF
065900030718     C                   ENDDO
066000030627      *
066100030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
066200030718      *  legato alla tabella EVO.
066300030627     c                   z-add     1             Ic                3 0
066400030718     c     Gruppo        lookup    Grp(Ic)                                21
066500030627     c  n21              z-add     1             Ic
066600030630     c  n21*blank        lookup    Grp(Ic)                                21
066700030718     c                   movel     Gruppo        Grp(Ic)
066800030627     c                   add       Importo       ImC(Ic)
066900030721      *
067000030721     c                   end
067100030703      *
067200030627     c                   Endsr
067300030627      *----------------------------------------------------------------
067400030627      *  Decodifica il Network
067500030627      *----------------------------------------------------------------
067600030627     c     Netw          Begsr
067700030627      *
067800030828     c                   clear                   ImpExp            1
067900030627      *
068000030627      *  x LNP controlla Estero
068100030627     C                   clear                   OG143
068200050907     c                   z-add     1             og
068300050907     c     tasLNP        lookup    Ofil(og)                               99
068400080404     c                   if        %Equal
068500050907     C                   movel     Ode3(og)      OG143
068600030627     c                   end
068700030627      *
068800030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
068900030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
069000030627      *  se non fosse risultato tale allora deve controllare x LNA.
069100030627     c                   if         §ogNTW = 'FED' or
069200030627     c                              §ogNTW = 'EEX' or
069300030627     c                              §ogNTW = 'DPD'
069400030627     c                   movel     §ogNTW        ECFNTW
069500030703      *  quindi le competenze da tenere in considerazione per il confronto sono
069600030703      *  di arrivo.
069700030828     c                   movel     'I'           ImpExp
069800030721     c                   movel     'LNP'         Estero_LN         3
069900030721     c                   z-add     tasLNP        Linea_Cod         3 0
070000030627      *
070100030627     c                   else
070200030627     C                   clear                   OG143
070300030703      * prova per LNA -> se prima, x LNP il network non è un estero
070400050907     c                   z-add     1             og
070500050907     c     tasLNA        lookup    Ofil(og)                               99
070600080404     c                   if        %Equal
070700050907     C                   movel     Ode3(og)      OG143
070800030627     c                   movel     §ogNTW        ECFNTW
070900030703      *  quindi le competenze da tenere in considerazione per il confronto sono
071000030703      *  di partenza.
071100030828     c                   movel     'E'           ImpExp
071200030721     c                   movel     'LNA'         Estero_LN
071300030721     c                   z-add     tasLNA        Linea_Cod
071400030627     c                   end
071500030627     c                   end
071600030627      *
071700030627     c                   Endsr
071800030627      *----------------------------------------------------------------
071900080617      *  x le bolle di reso risale la catena fino alla bolla Originale
072000080617      *----------------------------------------------------------------
072100080617     c     Cerca_la_MammaBegsr
072200080617      *
072300080617      * Con la chiave bolla di TITAS
072400080617      *  aggancia la figlia per cercare la madre.
072500080617     c     klbl1         chain     fnLBL01l
072600080617     c                   If        %Found(fnLBL01l)
072700080617      *
072800080617      *  se la bolla originale equivale alla madre
072900080617     c                   if        lblAAo = lblAAp and
073000080617     c                             lblLPo = lblLPp and
073100080617     c                             lblNRo = lblNRp and
073200080617     c                             lblNSo = lblNSp
073300080617      *  esce dalla routine
073400080617     c                   LEAVESR
073500080617     c                   end
073600080617      *
073700080617      * altrimenti prosegue alla ricerca della bolla mamma che sia l'originale
073800080617      *
073900080617     c     cerca_ancora  tag
074000080617      *
074100080617     c     klbl1_1       chain     fnLBL01l
074200080617     c                   If        %Found(fnLBL01l)
074300080617      *
074400080617      *  se la bolla originale equivale alla madre
074500080617     c                   if        lblAAo = lblAAp and
074600080617     c                             lblLPo = lblLPp and
074700080617     c                             lblNRo = lblNRp and
074800080617     c                             lblNSo = lblNSp
074900080617      *  esce dalla routine
075000080617     c                   LEAVESR
075100080617     c                   elSe
075200080617     c                   goTO      cerca_ancora
075300080617     c                   end
075400080617      *
075500080617     c                   elSe
075600080617     c                   endIf
075700080617      *
075800080617     c                   endIf
075900080617      *
076000080617     c                   Endsr
076100080617      *----------------------------------------------------------------
076200050907     c     *inzsr        Begsr
076300050907      *
076400050907      *  carica AZORG una volta per tutte:
076500050907     c                   clear                   oFil
076600050907     c                   clear                   oDe3
076700050907     c                   open      AzOrg01L
076800050907     c     *loval        setll     AzOrg01L
076900050907     c                   read      AzOrg01L
077000050907     c                   DOW       not %EoF(AzOrg01L)
077100050907     c                   add       1             og
077200050907     c                   move      orgfil        oFIL(og)
077300050907     c                   move      orgde3        oDE3(og)
077400050907     c                   read      AzOrg01L
077500050907     c                   enddo
077600050907     c                   close     AzOrg01L
077700050907      *
077800050907     c                   Endsr
077900050907      *----------------------------------------------------------------
