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
001400040830     FTNECF02L  uF a E           k DISK
001500040902     FTNECF01L  uF   E           k DISK    rename(TNECF000:TNECF1)
001600081022     FTNECF04L  uF   E           k DISK    rename(TNECF000:TNECF4) prefix(EXF:3)
001700030620      *
001800081022     D Scrivere        S              2
001900091001     D Importo         S             13  5
002000030630     D GRo             S              3    DIM(50) descend
002100030630     D GRp             S              3    DIM(50)
002200091001     D IMr             S             16  5 DIM(50)
002300091001     D IMc             S             16  5 DIM(50)
002400091001     D IMp             S             16  5 DIM(50)
002500040830     D Dfp             S              8  0 DIM(50)
002600040830     D Nfp             S             15    DIM(50)
002700030718      *
002800030721      * Importo Voci spedizione (0)
002900030721    >D EcceV0        E DS                  extname(eccev00f)
003000030721     D Ptc0            S               *   INZ(%ADDR(CEVC01))
003100030721     D CodV0           S              3  0 DIM(50)
003200030721     D                                     BASED(Ptc0)
003300030721     D Pti0            S               *   INZ(%ADDR(CEVI01))
003400090910     D ImpV0           S             13  5 DIM(50)
003500030721     D                                     BASED(Pti0)
003600030721      * Importo Voci spedizione (P)
003700030721    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
003800030721     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
003900030721     D CodVP           S              3  0 DIM(50)
004000030721     D                                     BASED(Ptcv)
004100030721     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
004200090910     D ImpVP           S             13  5 DIM(50)
004300030721     D                                     BASED(PtiV)
004400030627      *
004500030624     D KPJBA         E DS
004600030625     D OG143         E DS
004700070927     D DSbl4E        E DS
004800030625      *
004900030625     D WLBDAT          DS
005000030625     D  G02DAT                 1      8  0
005100030625     D  G02INV                 9     16  0
005200030625     D  G02ERR                17     17
005300030625     D  G02TGI                18     22  0
005400040902      *
005500040902     D  data_Oggi      s                   like(G02INV) INZ(0)
005600030624      *
005700030624     D param           DS
005800030624     D   kAas                  1      4  0
005900030624     D   kLnp                  5      7  0
006000030624     D   kNrs                  8      9  0
006100030624     D   kNsp                 10     16  0
006200030624     D   kTbl                 17     18
006300030703     D   kNtw                 19     21
006400060303      *
006500060303     D TAS_Bolla       DS
006600060303     D   sTASaas               1      4  0
006700060303     D   sTASLnp               5      7  0
006800060303     D   sTASNrs               8      9  0
006900060303     D   sTASNsp              10     16  0
007000060303     D   TASBolla14            3     16  0
007100030721      *
007200050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
007300050907      *
007400050907     D  Ofil           s                   like(orgfil) DIM(999)
007500050907     D  Ode3           s                   like(orgde3) DIM(999)
007600050907     d  og             s              5  0
007700030721      *---------------------------------------------------------------------*
007800030721     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
007900030721     IECCEV000      01
008000030721     IECCEVP00      02
008100030620      *---------------------------------------------------------------------*
008200030624     C     *entry        plist
008300030624     C                   parm                    kpjba
008400050907     C                   parm                    DsTAS
008500050907      **** la DsTAS sostituisce la lettura dello stesso record del TITAS
008600050907      **** avendo il pgm già selezionato il record da elaborare nel FIEU78R.
008700030624     c                   movel     kpjbu         param
008800030627      *
008900030627     c     ktbe          klist
009000030627     c                   kfld                    tbeCOD
009100030627     c                   kfld                    tbeKE1
009200030627     c                   kfld                    tbeKE2
009300030718      *
009400030718     c     kEVA          klist
009500030722     c                   kfld                    evaVAR
009600030722     c                   kfld                    evaNTW
009700030722     c                   kfld                    evaFIL
009800030718      *
009900030718     c     kEVO          klist
010000030722     c                   kfld                    evoNTW
010100030722     c                   kfld                    evoFIL
010200030721     c                   kfld                    evoVOC
010300030627      *
010400030627     c     ktas1         klist
010500030627     c                   kfld                    kAas
010600030624     c                   kfld                    kLnp
010700030624     c                   kfld                    kNrs
010800030624     c                   kfld                    kNsp
010900030624      *
011000030627     c     ktas2         klist
011100030624     c                   kfld                    kAas
011200030624     c                   kfld                    kLnp
011300030624     c                   kfld                    kNrs
011400030624     c                   kfld                    kNsp
011500030624     c                   kfld                    kTbl
011600030625      *
011700030627     c     ktas3         klist
011800030625     c                   kfld                    tasAas
011900030625     c                   kfld                    tasLnp
012000030625     c                   kfld                    tasNrs
012100030625     c                   kfld                    tasNsp
012200030625     c                   kfld                    tasTbl
012300040830      *
012400040830     c     kECF          klist
012500040830     c                   kfld                    tasAas
012600040830     c                   kfld                    tasLnp
012700040830     c                   kfld                    tasNrs
012800040830     c                   kfld                    tasNsp
012900040902      *
013000040902     c     kECF1         klist
013100040902     c                   kfld                    ecfntw
013200040902     c                   kfld                    ecfrif
013300081022      *
013400081022     c     kECF4         klist
013500081022     c                   kfld                    ecfntw
013600081022     c                   kfld                    ecfrif
013700081022     c                   kfld                    ecfgrp
013800050907      ***
013900050907     c***                if        kTbl = *blank
014000050907     c***  ktas1         setll     titas30c
014100050907     c***  ktas1         reade     titas30c
014200050907     c***                else
014300050907     c***  ktas2         setll     titas30c
014400050907     c***  ktas2         reade     titas30c
014500050907     c***                end
014600050907      ***
014700050907     c***                dow       not %Eof(titas30c)
014800030625      *
014900050907      *            * ------------------------------- *
015000030627     c                   exsr      campi_ECF
015100050907      *            * ------------------------------- *
015200050907      *
015300050907      ***
015400050907     c***                if        kTbl = *blank
015500050907     c***  ktas1         reade     titas30c
015600050907     c***                else
015700050907     c***  ktas2         reade     titas30c
015800050907     c***                end
015900050907      ***
016000050907     C***                Enddo
016100050907      ***
016200050907     c***                seton                                        LR
016300050907     c                   seton                                        RT
016400030627      *----------------------------------------------------------------
016500030627      * imposta i campi per la scrittura
016600030627      *----------------------------------------------------------------
016700030627     c     campi_ECF     Begsr
016800040830      *
016900040830     c                   clear                   Gro
017000040830     c                   clear                   Grp
017100040830     c                   clear                   ImR
017200040830     c                   clear                   ImC
017300040830     c                   clear                   ImP
017400040830     c                   clear                   DfP
017500040830     c                   clear                   NfP
017600040902     **
017700040902     **  controlla che non esistessero precedenente dei records incompleti
017800040902     **   ossia non ancora fatturati da Bartolini e già addebitati dal Partner
017900040908     **   oppure non presenti nel sistema Bartolini e già addebitati dal Partner
018000040902     c                   exsr      chk_e_delete
018100040902     **
018200040902      *-------------------
018300040830     c                   clear                   tnecf000
018400040116      *
018500040116      * Decodifica il Network
018600040116     c                   exsr      NetW
018700040909      *
018800040909      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
018900040909      *   per poter distinguere di quale Partner si tratta
019000060303     c                   clear                   savNTW            3 0
019100040909     c                   if         ecfNTW = 'EEX'
019200040909     c                   move      Linea_Cod     ecfNTW
019300060303     c                   move      Linea_Cod     savNTW            3 0
019400040909     c                   end
019500040909      *
019600040116     c                   if        KNtw <> *blank and
019700040116     c                             Kntw <> ecfNTW
019800040116     c                   goto      No_write
019900040116     c                   End
020000030627      *  imposta UDATE
020100030627     C                   move      *date         G02DAT
020200030627     C                   MOVE      *ZEROS        G02INV
020300030627     C                   MOVE      *BLANKS       G02ERR
020400030627     C                   CALL      'XSRDA8'
020500030627     C                   PARM                    WLBDAT
020600040902      *  è diventata la data consolidamento non più la data elaborazione
020700040902     C                   Z-ADD     G02INV        data_oggi
020800040902     C*********          Z-ADD     G02INV        ECFDTG
020900030627      *
021000030627     c                   movel     tasAAS        ECFAAS
021100030627     c                   movel     tasLNP        ECFLNP
021200030627     c                   movel     tasNRS        ECFNRS
021300030627     c                   movel     tasNSP        ECFNSP
021400030627     c                   movel     tasLNA        ECFLNA
021500030627     c                   movel     tasMGS        ECFMGS
021600030627     c                   movel     tasTBL        ECFTBL
021700030627     c                   movel     tasFBR        ECFFBR
021800030627     c                   movel     tasKSC        ECFKSC
021900030627     c                   movel     tasDTI        ECFDTI
022000030627     c                   movel     tasDCM        ECFDCM
022100030627     c                   movel     tasCCA        ECFCCA
022200030627     c                   movel     tasNCL        ECFNCL
022300030627     c                   movel     tasPKF        ECFPKF
022400030627     c                   movel     tasVLF        ECFVLF
022500040902     c                   z-add     tasRMN        ECFRMN
022600160527      **
022700160527     c                   if        tasNZD <>*blank
022800160527     c                   movel     tasNZD        ECFNAZ
022900160527     c                   elseIf    tasNZM <>*blank
023000160527     c                   movel     tasNZM        ECFNAZ
023100160527     c                   end
023200160527      **
023300160527     c                   movel     tasCTS        ECFCTS
023400160527     c                   movel     tasFIN        ECFFIN
023500160527     c                   movel     tasFTC        ECFFTC
023600160527     c                   movel     tasTC2        ECFTC2
023700030703      *
023800030703      * dta e nr.fattura del Partner
023900030707     c                   movel     tasNFT        ECFNFT
024000030707     c                   movel     tasDFT        ECFDFT
024100030627      *
024200030627     c                   exsr      Rif_Partner
024300030630      *
024400040902     c                   if        savdta = 0
024500040830     c                   movel     *zeros        ECFDTA
024600040902     c                   else
024700040902     C                   Z-ADD     data_oggi     ecfdta
024800040909     C                   move      '28'          ecfdta
024900040902     c                   end
025000040902      *
025100030627      * Imposta i Ricavi
025200050216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
025300050216     c                             tastbl <> 'AP'
025400030627     c                   exsr      Ricavi
025500050216     c                   end
025600050216      *
025700030627      * Imposta le Competenze
025800030627     c                   exsr      Competenze
025900030627      *
026000030630      * riordina i dati immagazzinati per scrivere i records
026100030630     c                   movea     grp           gro
026200030630     c                   sorta     gro
026300030630      *   deve scrivere tanti records quanti sono i raggruppamenti messi
026400030630      *    nelle schiere
026500030630     c                   do        50            cnt               3 0
026600041015      *
026700041015     c                   select
026800041015      *
026900041015     c                   when      gro(1) = *blank and cnt = 1
027000041015     c                   move      '001'         gro(1)
027100041015      *
027200041015     c                   when      gro(cnt) = *blank and cnt > 1
027300030630     c                   leave
027400041015      *
027500041015     c                   endsl
027600030627      *
027700030723     c                   movel     gro(cnt)      ECFGRP
027800030630      *
027900030630     c                   z-add     1             Ir
028000030630     c     gro(cnt)      lookup    Grp(Ir)                                21
028100030630      * Ricavi/Competenze
028200091001     c   21              z-add(h)  ImR(Ir)       ECFIMR
028300091001     c   21              z-add(h)  ImC(Ir)       ECFIMC
028400040830      *
028500091001     c   21              z-add(h)  ImP(Ir)       ECFIMP
028600041015     c   21              z-add     DfP(Ir)       ECFDFP
028700041015     c   21              movel     NfP(Ir)       ECFNFP
028800030627      *
028900081022     c                   eval      scrivere = 'SI'
029000081022      *  solo x DPD:
029100081022      *  se presente lo stesso riferimento su altra bolla non ancora CONSOLIDATA
029200081022      *  si deve attribuire Ricavo e Competenza su quella altrimenti si scrive
029300081023     c                   if        ecfNTW = 'DPD' and ecfrif <> *blank
029400081022     c     kECF4         chain     TNECF04L
029500081022     c                   if        %Found(TNECF04L) and eXfdtg=0
029600081022      *  somma ricavi e competenze
029700081022     c                   add       ecfimR        eXfimR
029800081022     c                   add       ecfimC        eXfimC
029900081022     c                   update    tnecf4
030000081022     c                   eval      scrivere = 'NO'
030100081022     c                   end
030200081022     c                   end
030300081022      *
030400081022     c                   if        scrivere = 'SI'
030500091022     c                   if        ecfNTW = 'DPD' and ecfrif <> *blank
030600091022     c                   move(p)   1             EcfNFp
030700091022     c                   end
030800040830     c                   write     tnecf000
030900081022     c                   end
031000030630      *
031100030630     c                   EndDO
031200030703     c     No_Write      tag
031300030627      *
031400030627     c                   Endsr
031500030627      *----------------------------------------------------------------
031600040902      *  Controlla e cancella records anomali precedentemente scritti
031700030627      *----------------------------------------------------------------
031800040902     c     chk_e_delete  Begsr
031900040902      *
032000040902     c                   clear                   savdta            8 0
032100030829      *
032200040902      * Per prima cosa:
032300040902      *  Controlla che non fosse già presente un record non ancora fatturato
032400040902     c     kecf          setll     tnecf02l
032500040902     c     kecf          reade     tnecf02l
032600040902      *
032700040902     c                   dow       not %eof(tnecf02l)
032800040902      *
032900040902      *  Non deve essere Consolidato
033000040902     c                   if        ecfdtg = *zero
033100040902      * se era stato scritto precedentemente
033200040902     c                   if        ecftpr ='NFT'
033300040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
033400040902      *  legato alla tabella EVA.
033500040902     c                   z-add     1             Ir                3 0
033600040902     c     ecfGRP        lookup    Grp(Ir)                                21
033700040902     c  n21              z-add     1             Ir
033800040902     c  n21*blank        lookup    Grp(Ir)                                21
033900040902     c                   movel     ecfGRP        Grp(Ir)
034000040902     c                   add       ecfIMP        ImP(Ir)
034100040902     c                   movel     ecfNFP        NfP(Ir)
034200040902     c                   z-add     ecfDFP        DfP(Ir)
034300040908     c                   z-add     data_oggi     savdta
034400040902     c                   delete    tnecf000
034500040902     c                   end
034600040902     c                   end
034700040902      *
034800040902     c     kecf          reade     tnecf02l
034900040902     c                   end
035000040902      *
035100040902      * Per seconda cosa:
035200040902      *  Controlla che fosse presente un record non trovato
035300040902      *
035400040902      * Decodifica il Network
035500040902     c                   exsr      NetW
035600040902      *  Se si tratta di Partners Euroexpress nel NTW imposto la Linea
035700040902      *   per poter distinguere di quale Partner si tratta
035800040902     c                   if         ecfNTW = 'EEX'
035900040902     c                   move      Linea_Cod     ecfNTW
036000040902     c                   end
036100040909      *
036200040902     c                   exsr      Rif_Partner
036300040902      *
036400040902     c     kecf1         setll     tnecf01l
036500040902     c     kecf1         reade     tnecf01l
036600040902      *
036700040902     c                   dow       not %eof(tnecf01l)
036800040902      *
036900040902      *  Non deve essere Consolidato
037000040902     c                   if        ecfdtg = *zero
037100040902      * se era stato scritto precedentemente
037200040902     c                   if        ecftpr ='NTR'
037300040902      * Incasella l'importo del ricavo nel gruppo di appartenenza
037400040902      *  legato alla tabella EVA.
037500040902     c                   z-add     1             Ir                3 0
037600040902     c     ecfGRP        lookup    Grp(Ir)                                21
037700040902     c  n21              z-add     1             Ir
037800040902     c  n21*blank        lookup    Grp(Ir)                                21
037900040902     c                   movel     ecfGRP        Grp(Ir)
038000040902     c                   add       ecfIMP        ImP(Ir)
038100040902     c                   movel     ecfNFP        NfP(Ir)
038200040902     c                   z-add     ecfDFP        DfP(Ir)
038300040908     c                   z-add     data_oggi     savdta
038400040902     c                   delete    tnecf1
038500040902     c                   end
038600040902     c                   end
038700040902      *
038800040902     c     kecf1         reade     tnecf01l
038900040902     c                   end
039000040902      *
039100040902     c                   Endsr
039200040902      *----------------------------------------------------------------
039300040902      *  Riferimento Partner Estero solo se una Partenza
039400040902      *----------------------------------------------------------------
039500040902     c     Rif_Partner   Begsr
039600040902      *
039700030627      *  RIFERIMENTO Mittente  da tita4
039800030627     c     ktas1         setll     tita430c
039900030627     c     ktas1         reade     tita430c
040000030627      *
040100030627     c                   dow       not %Eof(tita430c)
040200030627      *
040300030829      * Se è un DPD x LNP/Network
040400030828     c                   if        §ogNTW = 'DPD'
040500030828      *
040600070619      *  x DPD deve prendere da TITA4 il record 'I'
040700070619     c                   if        ta4TRC = 'I'
040800080414     c                   eval      ECFRIF =  %subst(ta4not:1:14)
040900080414      *
041000080414      *
041100080414      * NON VA + BENE si salta la compattazione:
041200080414     c                   goto      NOcompatta
041300080414      *
041400070619     c                   if        %subst(ta4not:1:1) = '0' and
041500070619     c                             %subst(ta4not:5:2) = '99'
041600070619     c                   eval      fld11 = %subst(ta4not:2:3) +
041700070619     c                                     %subst(ta4not:7:8)
041800070619     c                   else
041900060626     c                   eval      ECFRIF =  %subst(ta4not:1:14)
042000030627     c                   end
042100080414      *
042200080414     c     NOcompatta    tag
042300080414      *
042400070619     c                   end
042500030828      *
042600030828     c                   else
042700030828      *
042800030828      * Se è un Fedex x LNP/Network
042900070927     c*****              if        ta4TRC = 'A'
043000070927     c                   if        ta4TRC = 'E'
043100060303      *
043200070927     c****               eval      ECFRIF = %subst(ta4not:16:20)
043300070927     c                   movel     ta4not        dsbl4E
043400070927     c                   movel     §b4ERP        ecfRIF
043500060303      **
043600060303      * ?Attenzione se si tratta di EUROEXPRESS Export il riferimento
043700060303      * ?  è la nostra bolla   NON DPD o FED --> ossia savNTW diverso da 0
043800060303     c                   if        savNTW <> 0
043900060303      * ? x Export il NTW deve essere uguale alla linea Arrivo
044000060303     c                   if        savNTW = ecfLNA or ecfRIF = *blank
044100060303     c                   eval      stasaas = tasaas
044200060303     c                   eval      staslnp = taslnp
044300060303     c                   eval      stasnrs = tasnrs
044400060303     c                   eval      stasnsp = tasnsp
044500060303     c                   movel     tasBolla14    ecfRIF
044600060303     c                   end
044700060303      *
044800060303     c                   end
044900060303      *
045000030828      *  se non c'è il riferimento allora imposta il rif.numerico
045100030828     c                   if        ecfRIF = *blank
045200040902     c                   movel     tasRMN        ecfRIF
045300030828     c                   end
045400060303      *
045500030828     c                   end
045600030828      *
045700030828     c                   end
045800030627      *
045900030627     c     ktas1         reade     tita430c
046000030627     c                   enddo
046100030828      *
046200030828      *  se Non trova un riferimento prende il rif_numerico
046300030828     c                   if        ecfRIF = *blank
046400060303      *
046500030828     c                   if        §ogNTW = 'DPD'
046600070619      *
046700070619     c                   select
046800070619      *
046900070619     c                   when      tasRMN > 100000000000 and
047000070619     c                             tasRMN < 999999999999
047100070619     c                   move      tasRMN        fld12            12
047200070619     c                   movel     fld12         fld11            11
047300070619     c                   movel(p)  fld11         ecfRIF
047400070619      *
047500070619     c                   when      tasRMN > 999999999999  and
047600070619     c                             tasRMN < 99999999999999
047700070619     c                   move      tasRMN        fld14            14
047800070619     c                   movel(p)  fld14         ecfRIF
047900070619      *
048000070619     c                   endSL
048100070619      *
048200030828     c                   else
048300060303      **
048400060303      * ?Imposta come riferimento la nostra Bolla
048500060303     c                   eval      stasaas = tasaas
048600060303     c                   eval      staslnp = taslnp
048700060303     c                   eval      stasnrs = tasnrs
048800060303     c                   eval      stasnsp = tasnsp
048900060303     c                   movel     tasBolla14    ecfRIF
049000060303      **
049100030828     c                   end
049200030828     c                   end
049300030828      *
049400030829      *
049500030627     c                   Endsr
049600030627      *----------------------------------------------------------------
049700030627      *  imposta i Ricavi raggruppati su schiere
049800030627      *----------------------------------------------------------------
049900030627     c     Ricavi        Begsr
050000030627      *
050100030627      * IMPORTO Ricavo
050200030627      *  Porto
050300030627     c                   if        tasPOR > 0
050400030722     c                   clear                   evaVAR
050500030722     c                   movel     '1'           evaVAR
050600160126     c                   if        tasPOR  <> 8888888
050700030627     c                   z-add     tasPOR        importo
050800160126     c                   end
050900030718     c                   exsr      Tbe_EVA
051000030627     c                   end
051100030627      *  Varie 1
051200030627     c                   if        tasVA1 > 0
051300030722     c                   clear                   evaVAR
051400030722     c                   movel     tasSV1        evaVAR
051500160126     c                   if        tasVA1  <> 8888888
051600030627     c                   z-add     tasVA1        importo
051700160126     c                   end
051800030718     c                   exsr      Tbe_EVA
051900030627     c                   end
052000030627      *  Varie 2
052100030627     c                   if        tasVA2 > 0
052200030722     c                   clear                   evaVAR
052300030722     c                   movel     tasSV2        evaVAR
052400160126     c                   if        tasVA2  <> 8888888
052500030627     c                   z-add     tasVA2        importo
052600160126     c                   end
052700030718     c                   exsr      Tbe_EVA
052800030627     c                   end
052900030627      *  Varie 3
053000030627     c                   if        tasVA3 > 0
053100030722     c                   clear                   evaVAR
053200030722     c                   movel     tasSV3        evaVAR
053300160126     c                   if        tasVA3  <> 8888888
053400030627     c                   z-add     tasVA3        importo
053500160126     c                   end
053600030718     c                   exsr      Tbe_EVA
053700030627     c                   end
053800030627      *  Altre Varie
053900030627      *
054000030627      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
054100030627      *  e raggruppare.
054200030627     c     ktas3         setll     tita730c
054300030627     c     ktas3         reade     tita730c
054400030627     c                   dow       not %Eof(tita730c)
054500030722     c                   clear                   evaVAR
054600030722     c                   movel     ta7SVn        evaVAR
054700160126     c                   if        ta7VAn  <> 8888888
054800030627     c                   z-add     ta7VAn        importo
054900160126     c                   end
055000030718     c                   exsr      Tbe_EVA
055100030627     c     ktas3         reade     tita730c
055200030627     c                   enddo
055300030627      *
055400030627     c                   Endsr
055500030627      *----------------------------------------------------------------
055600030627      *  Decodifica il raggruppamento per le Varie
055700030627      *----------------------------------------------------------------
055800030718     c     TBE_EVA       Begsr
055900030627      *
056000030722     c                   clear                   evaNTW
056100030722     c                   clear                   evaFIL
056200030718     c                   move      '999'         Gruppo            3
056300030721      *
056400030721      * prova se ci sono delle particolarità sul Partner + LNP/A
056500030722      *  chiave completa
056600030722     c                   movel     §ogNtw        evaNTW
056700030722     c                   move      Linea_Cod     evaFIL
056800030721     c     kEVA          setll     tnEVA01l
056900030721     C                   IF        not %EQUAL
057000030721      * se ci sono dei codici particolari da prendere in considerazione
057100030627      *  al posto di quelli standard.
057200030722      *  chiave parziale
057300030722     c                   movel     §ogNtw        evaNTW
057400030722     c                   clear                   evaFIL
057500030718     c     kEVA          setll     tnEVA01l
057600030718     C                   IF        not %EQUAL
057700030722     c                   clear                   evaNTW
057800030722     c                   clear                   evaFIL
057900030722     c     kEVA          setll     tnEVA01l
058000030718     c                   end
058100030721     c                   endIF
058200030627      *
058300030718     C                   IF        %EQUAL
058400030718      *
058500030718     C                   DOU       %EOF
058600030718     C     kEVA          Reade     tnEVA01l
058700030718     C                   IF        NOT %EOF and
058800040902     C                             evaDDA <= data_oggi and
058900040902     C                             evaADA >= data_oggi
059000030718     c                   move      evaGRP        Gruppo
059100030718     c                   leave
059200030718     C                   ENDIF
059300030718     C                   ENDDO
059400030718      *
059500030718     c                   end
059600030718      *
059700030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
059800030718      *  legato alla tabella EVA.
059900030627     c                   z-add     1             Ir                3 0
060000030718     c     Gruppo        lookup    Grp(Ir)                                21
060100030627     c  n21              z-add     1             Ir
060200030630     c  n21*blank        lookup    Grp(Ir)                                21
060300030718     c                   movel     Gruppo        Grp(Ir)
060400030627     c                   add       Importo       ImR(Ir)
060500030627      *
060600030627     c                   Endsr
060700030627      *----------------------------------------------------------------
060800030627      *  imposta le Competenze raggruppate su schiere
060900030627      *----------------------------------------------------------------
061000030627     c     Competenze    Begsr
061100030627      *
061200030721     c                   clear                   ImpV0
061300030721     c                   clear                   ImpVP
061400030721     c                   clear                   CodV0
061500030721     c                   clear                   CodVP
061600030721      *
061700030627      * IMPORTO Competenze
061800030718     c     ktas3         chain     Eccev30C
061900030718     c                   if        %Found(Eccev30C)
062000030627      *
062100030721     C                   Do        50            CV                3 0
062200030721      *
062300030721     C                   If        *in01 = *on
062400030721      * esce quando non ha più Voci in skiera
062500030721     C                   If        CodV0(CV) = 0
062600030721     c                   leave
062700030721     c                   end
062800030721     c                   clear                   evoVOC
062900030721     c                   movel     CodV0(CV)     evoVOC
063000030721     c                   z-add     ImpV0(CV)     importo
063100030721     c                   exsr      Tbe_EVO
063200030721     c                   end
063300030718      *
063400030721     C                   If        *in02 = *on
063500030721      * esce quando non ha più Voci in skiera
063600030721     C                   If        CodVP(CV) = 0
063700030721     c                   leave
063800030721     c                   end
063900030721     c                   clear                   evoVOC
064000030721     c                   movel     CodVP(CV)     evoVOC
064100030721     c                   z-add     ImpVP(CV)     importo
064200030721     c                   exsr      Tbe_EVO
064300030721     c                   end
064400030721      *
064500030721     c                   endDo
064600030627      *
064700030721     C                   endIF
064800030721      *
064900030627     c                   Endsr
065000030627      *----------------------------------------------------------------
065100030627      *  Decodifica il raggruppamento per le Voci
065200030627      *----------------------------------------------------------------
065300030718     c     TBE_EVO       Begsr
065400030722      *
065500030722     c                   clear                   evoNTW
065600030722     c                   clear                   evoFIL
065700030722     c                   move      '999'         Gruppo            3
065800030722      *
065900030722      * prova se ci sono delle particolarità sul Partner + LNP/A
066000030722      *  chiave completa
066100030722     c                   movel     §ogNtw        evoNTW
066200030722     c                   move      Linea_Cod     evoFIL
066300030718     c     kEVO          setll     tnEVO01l
066400030718     C                   IF        not %EQUAL
066500030722      * se ci sono dei codici particolari da prendere in considerazione
066600030722      *  al posto di quelli standard.
066700030722      *  chiave parziale
066800030723     c                   movel     §ogNtw        evoNTW
066900030723     c                   clear                   evoFIL
067000030718     c     kEVO          setll     tnEVO01l
067100030718     C                   IF        not %EQUAL
067200030723     c                   clear                   evoNTW
067300030723     c                   clear                   evoFIL
067400030722     c     kEVO          setll     tnEVO01l
067500030718     c                   end
067600030718     c                   endIF
067700030718      *
067800030718     C                   IF        %EQUAL
067900030718      *
068000030718     C                   DOU       %EOF
068100030718     C     kEVO          Reade     tnEVO01l
068200030718     C                   IF        NOT %EOF        and
068300040902     C                             evoDDA <= data_oggi and
068400040902     C                             evoADA >= data_oggi
068500030718     c                   move      evoGRP        Gruppo
068600030718     c                   leave
068700030718     C                   ENDIF
068800030718     C                   ENDDO
068900030627      *
069000030627      * Incasella l'importo del ricavo nel gruppo di appartenenza
069100030718      *  legato alla tabella EVO.
069200030627     c                   z-add     1             Ic                3 0
069300030718     c     Gruppo        lookup    Grp(Ic)                                21
069400030627     c  n21              z-add     1             Ic
069500030630     c  n21*blank        lookup    Grp(Ic)                                21
069600030718     c                   movel     Gruppo        Grp(Ic)
069700030627     c                   add       Importo       ImC(Ic)
069800030721      *
069900030721     c                   end
070000030703      *
070100030627     c                   Endsr
070200030627      *----------------------------------------------------------------
070300030627      *  Decodifica il Network
070400030627      *----------------------------------------------------------------
070500030627     c     Netw          Begsr
070600030627      *
070700030828     c                   clear                   ImpExp            1
070800030627      *
070900030627      *  x LNP controlla Estero
071000030627     C                   clear                   OG143
071100050907     c                   z-add     1             og
071200050907     c     tasLNP        lookup    Ofil(og)                               99
071300080404     c                   if        %Equal
071400050907     C                   movel     Ode3(og)      OG143
071500030627     c                   end
071600030627      *
071700030627      * Attenzione: il Network estero può essere x LNP o x LNA a seconda
071800030627      *  si tratti di IMPORT o EXPORT quindi avendo prima controllato per LNP,
071900030627      *  se non fosse risultato tale allora deve controllare x LNA.
072000030627     c                   if         §ogNTW = 'FED' or
072100030627     c                              §ogNTW = 'EEX' or
072200030627     c                              §ogNTW = 'DPD'
072300030627     c                   movel     §ogNTW        ECFNTW
072400030703      *  quindi le competenze da tenere in considerazione per il confronto sono
072500030703      *  di arrivo.
072600030828     c                   movel     'I'           ImpExp
072700030721     c                   movel     'LNP'         Estero_LN         3
072800030721     c                   z-add     tasLNP        Linea_Cod         3 0
072900030627      *
073000030627     c                   else
073100030627     C                   clear                   OG143
073200030703      * prova per LNA -> se prima, x LNP il network non è un estero
073300050907     c                   z-add     1             og
073400050907     c     tasLNA        lookup    Ofil(og)                               99
073500080404     c                   if        %Equal
073600050907     C                   movel     Ode3(og)      OG143
073700030627     c                   movel     §ogNTW        ECFNTW
073800030703      *  quindi le competenze da tenere in considerazione per il confronto sono
073900030703      *  di partenza.
074000030828     c                   movel     'E'           ImpExp
074100030721     c                   movel     'LNA'         Estero_LN
074200030721     c                   z-add     tasLNA        Linea_Cod
074300030627     c                   end
074400030627     c                   end
074500030627      *
074600030627     c                   Endsr
074700030627      *----------------------------------------------------------------
074800050907     c     *inzsr        Begsr
074900050907      *
075000050907      *  carica AZORG una volta per tutte:
075100050907     c                   clear                   oFil
075200050907     c                   clear                   oDe3
075300050907     c                   open      AzOrg01L
075400050907     c     *loval        setll     AzOrg01L
075500050907     c                   read      AzOrg01L
075600050907     c                   DOW       not %EoF(AzOrg01L)
075700050907     c                   add       1             og
075800050907     c                   move      orgfil        oFIL(og)
075900050907     c                   move      orgde3        oDE3(og)
076000050907     c                   read      AzOrg01L
076100050907     c                   enddo
076200050907     c                   close     AzOrg01L
076300050907      *
076400050907     c                   Endsr
076500050907      *----------------------------------------------------------------
