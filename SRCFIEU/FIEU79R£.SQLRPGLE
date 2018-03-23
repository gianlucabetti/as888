000100030620     ************************************************************************
000200030829      *   Confronta le Fatture ricevute con i dati Bartolini di Fatturazione
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600130717     FTnEfr01L  uF   E           k DISK    rename(TNefr000:TNefr1)
000700130717     FTnEfr02L  uF   E           k DISK    rename(TNefr000:TNefr2)
000800040608      *---------------------------------------------------------------------*
000900040818     FTitas30C  iF   E           k DISK    rename(TiTAs000:TiTAS0)
001000040818     F                                     rename(TiTAs010:TiTAS1)
001100040818     F                                     rename(TiTAsp00:TiTASP)
001200040608     FTitas32C  iF   E           k DISK
001300060706     **
001400150323     FFidpo06L  iF   E           k DISK
001500150323     **
001600060706     FTita437C  iF   E           k DISK
001700060706     **
001800070927     FTita438C  iF   E           k DISK    rename(TiTA4000:TiTA40)
001900040608     f                                     rename(TiTA4P00:TiTA4P)
002000151215     **
002100151215     FTita432C  iF   E           k DISK    rename(TiTA4000:TiTA4032)
002200151215     f                                     rename(TiTA4P00:TiTA4P32)
002300151216     **
002400151216     FTita730C  iF   E           k DISK
002500151216     FTneVA01L  iF   E           k DISK
002600151216     FTneVO01L  iF   E           k DISK
002700151216     FEcCev30C  iF   E           k DISK
002800151216     **
002900030723      *---------------------------------------------------------------------*
003000041103     FTnEcf04L  uF a E           k DISK    rename(TNecf000:TNecf4)
003100030703      *---------------------------------------------------------------------*
003200160205     D DsDPO         E DS                  EXTNAME(FIDPO00F)
003300160205     d                                     prefix(Sql_)
003400160203     D DsTAS         E DS                  EXTNAME(TITAS00F)
003500160205     d                                     prefix(Sql_)
003600160204     D DsTA4         E DS                  EXTNAME(TITA400F)
003700160205     d                                     prefix(Sql_)
003800160204      *
003900030901     D TNEcf_read    e DS                  EXTNAME(TNecf00F)
004000030901     D TNEcf_sav     e DS                  EXTNAME(TNecf00F)
004100030901     d                                     prefix(Sav_)
004200030625      *
004300030901     D KPJBA         E DS
004400030901      *
004500030625     D WLBDAT          DS
004600030625     D  G02DAT                 1      8  0
004700030625     D  G02INV                 9     16  0
004800030625     D  G02ERR                17     17
004900030625     D  G02TGI                18     22  0
005000030723      *
005100041011     D PAR_79r1        DS
005200041011     D   kAas                         4  0 INZ(0)
005300041011     D   kLnp                         3  0 INZ(0)
005400041011     D   kNrs                         2  0 INZ(0)
005500041011     D   kNsp                         7  0 INZ(0)
005600041011     D   kTpr                         3A   INZ(' ')
005700041011     D   kNfp                        15A   INZ(' ')
005800041011     D   kDfp                         8  0 INZ(0)
005900041011     D   kGrp                         3A   INZ(' ')
006000041011     D   kImp                        13  3 INZ(0)
006100030902      *
006200041011     D PAR_79r2        DS
006300041011     D   pAas                         4  0
006400041011     D   pLnp                         3  0
006500041011     D   pNrs                         2  0
006600041011     D   pNsp                         7  0
006700041011     D   pGrp                         3A
006800041011     D   pNft                         6  0
006900041011     D   pDft                         8  0
007000041011     D   pImr                        13  3
007100041011     D   pImc                        13  3
007200041108      *
007300041108     D PAR_Entry       DS
007400041108     D   d1ntw                        3
007500041108     D   d1Amg                        8  0
007600150323      *
007700150323     D RIF_fidpORM     DS
007800150323     D   DPOPOE
007900150323     D   DPONRS
008000150323     D   DPONOR
008100150323     D   DPONRV
008200160203      *
008300160203     D parBolla        DS
008400160203     D   tasAASprm             1      4  0
008500160203     D   tasLnpprm             5      7  0
008600160203     D   tasNrsprm             8      9  0
008700160203     D   tasNspprm            10     16  0
008800160203     D   tasTblprm            17     18
008900160203     D   KNtw                 19     21
009000041011      *
009100040818     D  ecfBLK         s                   like(Ecfgrp) INZ('   ')
009200040818     D  aas_SPED       s                   like(TASaas) INZ(0)
009300040818     D  lnp_SPED       s                   like(TASlnp) INZ(0)
009400040818     D  nrs_SPED       s                   like(TASnrs) INZ(0)
009500040818     D  nsp_SPED       s                   like(TASnsp) INZ(0)
009600040818     D  data_Oggi      s                   like(G02INV) INZ(0)
009700041104     D  ex_ecftpr      s                   like(ecftpr)
009800080618     D  saveRIF        s                   like(efrRIF)
009900151216      *
010000151216     D Importo         S             13  5
010100151216      *
010200151216     D GRp             S              3
010300151216     D IMr             S             16  5
010400151216     D IMc             S             16  5
010500151216      *
010600151216      * Importo Voci spedizione (0)
010700151216    >D EcceV0        E DS                  extname(eccev00f)
010800151216     D Ptc0            S               *   INZ(%ADDR(CEVC01))
010900151216     D CodV0           S              3  0 DIM(50)
011000151216     D                                     BASED(Ptc0)
011100151216     D Pti0            S               *   INZ(%ADDR(CEVI01))
011200151216     D ImpV0           S             13  5 DIM(50)
011300151216     D                                     BASED(Pti0)
011400151216      * Importo Voci spedizione (P)
011500151216    >D EcceVP        E DS                  extname(eccevP0f) prefix(P_)
011600151216     D Ptcv            S               *   INZ(%ADDR(P_CEVC01))
011700151216     D CodVP           S              3  0 DIM(50)
011800151216     D                                     BASED(Ptcv)
011900151216     D PtiV            S               *   INZ(%ADDR(P_CEVI01))
012000151216     D ImpVP           S             13  5 DIM(50)
012100151216     D                                     BASED(PtiV)
012200060303      *
012300060303     d Ds_Rif330       ds
012400060303     d  aas_Rif330             1      2
012500060303     d  lnp_Rif330             3      5
012600060303     d  nrs_Rif330             6      7
012700060303     d  nsp_Rif330             8     14
012800060303      *
012900060303     D  k_aas_SPED     s                   like(TASaas) INZ(0)
013000060303     D  k_lnp_SPED     s                   like(TASlnp) INZ(0)
013100060303     D  k_nrs_SPED     s                   like(TASnrs) INZ(0)
013200060303     D  K_nsp_SPED     s                   like(TASnsp) INZ(0)
013300060303      *
013400030723      *---------------------------------------------------------------------*
013500030723     d Digits          c                   Const('0123456789')
013600040818      *---------------------------------------------------------------------*
013700040818     iTITAS0        11
013800040818     iTITAS1        12
013900040818     iTITASP        13
014000151216      *---------------------------------------------------------------------*
014100151216     I* DEFINIZIONE DEI TIPI RECORD DEL FILE ECCEV30C
014200151216     IECCEV000      01
014300151216     IECCEVP00      02
014400030620      *---------------------------------------------------------------------*
014500030707      *   Ciclo principale
014600030707      *---------------------------------------------------------------------*
014700030624     C     *entry        plist
014800040818     c                   parm                    kpjba
014900041108     c                   movel     kpjbu         PAR_Entry
015000160203     c                   move      d1Amg         d1daAmg           8 0
015100160203     c                   move      '01'          d1daAmg
015200160203     c                   seton                                        LR
015300040818      *
015400150319      * SE DPD deve controllare che non manchino delle spedizioni da TITAS
015500150319      *  rispetto a quelle arrivate da DPD. A questo punto deve scrivere
015600150319      *  su TNECF le righe MANCANTI da comparare prima di effettuare
015700150319      *  l'INCASELLAMENTO.
015800150319      *   Ecco perchè l'aggiunta qui del programma di controllo e scrittura
015900150319      *   dei records mancanti da TITAS.
016000150319      * QUESTO VALE SOLO PER IL CONTROLLO CON IL CLEARING DPD.
016100150319      *   (ripassa i parametri ricevuti)
016200150319     c                   if        d1ntw = 'DPD'
016300150319     c                   call      'FIEU79R0'
016400150319     c                   parm                    kpjba
016500150319     c                   endIF
016600150319      *
016700040818      *  imposta UDATE di elaborazione
016800040818     C                   move      *date         G02DAT
016900040818     C                   MOVE      *ZEROS        G02INV
017000040818     C                   MOVE      *BLANKS       G02ERR
017100040818     C                   CALL      'XSRDA8'
017200040818     C                   PARM                    WLBDAT
017300040818     C                   Z-ADD     G02INV        data_oggi
017400030707      *
017500030829     C     Kecf1         Klist
017600030829     C                   Kfld                    EcfNTW
017700030829     C                   Kfld                    EcfRIF
017800030723     C                   Kfld                    EcfGRP
017900030902      *
018000030902     C     Kecf_blk      Klist
018100030902     C                   Kfld                    EcfNTW
018200030902     C                   Kfld                    EcfRIF
018300030902     C                   Kfld                    EcfBLK
018400030929      *
018500030929     C     Kecf_rif      Klist
018600030929     C                   Kfld                    EcfNTW
018700030929     C                   Kfld                    EcfRIF
018800040818      *
018900040818     C     KTas          Klist
019000040818     c                   Kfld                    aas_Sped
019100040818     c                   Kfld                    lnp_Sped
019200040818     c                   Kfld                    nrs_Sped
019300040818     c                   Kfld                    nsp_Sped
019400060303      *
019500060303     C     Ktas_Bolla    Klist
019600060303     c                   Kfld                    k_aas_Sped
019700060303     c                   Kfld                    k_lnp_Sped
019800060303     c                   Kfld                    k_nrs_Sped
019900060303     c                   Kfld                    k_nsp_Sped
020000151215      *
020100151215     C     KTasecf       Klist
020200151216     c                   Kfld                    ecfaas
020300151216     c                   Kfld                    ecflnp
020400151216     c                   Kfld                    ecfnrs
020500151216     c                   Kfld                    ecfnsp
020600151216      *
020700151216     c     ktas3         klist
020800151216     c                   kfld                    tasAas
020900151216     c                   kfld                    tasLnp
021000151216     c                   kfld                    tasNrs
021100151216     c                   kfld                    tasNsp
021200151216     c                   kfld                    tasTbl
021300060303      *
021400151216     c     kEVA          klist
021500151216     c                   kfld                    evaVAR
021600151216     c                   kfld                    evaNTW
021700151216     c                   kfld                    evaFIL
021800151216      *
021900151216     c     kEVO          klist
022000151216     c                   kfld                    evoNTW
022100151216     c                   kfld                    evoFIL
022200151216     c                   kfld                    evoVOC
022300030707      *
022400030707      * -------------------------------------------------------
022500130717     c     d1ntw         setll     tnEfr01l
022600130717     c     d1ntw         reade     tnEfr01l
022700030707      *
022800130717     c                   dow       not %Eof(tnEfr01l) and
022900041108     c                                  efrDFT <= d1Amg
023000030625      *
023100030723     c                   clear                   Trovato_rec       1
023200030829      *  con il riferimento
023300080618     C                   movel     EfrNTW        EcfNTW
023400080618     C                   movel     EfrRIF        EcfRIF
023500080618     C                   movel     EfrGRP        EcfGRP
023600080618     c*>>>>>>>>> DPD
023700080618     c                   IF        EfrNTW = 'DPD'
023800080618      * Quindi compone il Parcel come è stato scritto sul nostro sistema
023900080618      * se invece fosse di 14 caratteri
0240000806181    c                   if        %subst(efrrif:14:1) <>' '
024100080618     C     Kecf_rif      setll     tnecf04l
0242000806182    c                   if        not %EQUAL
024300080618     C                   movel     EfrRIF        EcfRIF
0244000806182-   c                   end
024500080618      * se invece fosse di 11 caratteri
0246000806181x   c                   elseIf    %subst(efrrif:12:1) = ' '
024700080618     C     Kecf_rif      setll     tnecf04l
0248000806182    c                   if        not %EQUAL
024900080618     c                   eval      ecfRIF = '0' + %subst(efrrif:1:3) +
025000080618     c                             '99' + %subst(efrrif:4:8)
025100080618     C     Kecf_rif      setll     tnecf04l
0252000806183    c                   if        not %EQUAL
025300080618     C                   movel     EfrRIF        EcfRIF
0254000806183-   c                   end
0255000806182-   c                   end
0256000806181-   c                   endIf
025700080618      *
025800080618      * Salva il riferimento calcolato
025900080618     C                   movel(p)  EcfRIF        SaveRIF
026000080618     c                   endIf
026100080618     c*>>>>>>>>> fine DPD
026200080618      *
026300030829     c                   exsr      Cerca_ECF1
026400030707      *
026500040818     C                   Z-ADD     data_Oggi     efrDTE
026600030723      *
026700030723      * causale di errore x non trovato
026800030929     c                   select
026900030929     c     Trovato_rec   wheneq    *blank
027000030911     C                   move      'SCA'         efrCAE
027100030929     c     Trovato_rec   wheneq    'W'
027200030911     C                   move      'WRT'         efrCAE
027300030929     c     Trovato_rec   wheneq    'G'
027400030929     C                   move      'GRP'         efrCAE
027500040818     c     Trovato_rec   wheneq    'F'
027600040818     C                   move      'NFT'         efrCAE
027700040818     c     Trovato_rec   wheneq    'N'
027800040818     C                   move      'NTR'         efrCAE
027900150323     c     Trovato_rec   wheneq    'O'
028000150323     C                   move      'ORM'         efrCAE
028100041012     c     Trovato_rec   wheneq    'S'
028200041012     C                   move      *blank        efrCAE
028300040819     **
028400030929     C                   Endsl
028500030723      *
028600030707     c                   update    TnEfr1
028700030707      *
028800130717     c     d1ntw         reade     tnEfr01l
028900030624     C                   Enddo
029000030707      *
029100160203      *  qui cerca gli ORM ITALIA-ITALIA solo BRT e non pagati da DPD
029200160203     c                   if        d1ntw = 'DPD'
029300160203     c                   exsr      ORM_ITA_BRT
029400160203     C                   End
029500160203      *
029600160203     c                   RETURN
029700030829      *----------------------------------------------------------------
029800030829      * Se NON c'era la spedizione
029900030829      *----------------------------------------------------------------
030000030829     c     Cerca_ECF1    Begsr
030100030829      *
030200041103     C     Kecf1         Chain     TnEcf04L
030300080618      *
030400040108      * Se trovato ------------>>   NTW/RIF/GRP
030500041103     c                   If        %Found(TnEcf04L)
030600030829      * cerca di associare
030700030829     c                   Exsr      Associa
030800030829      *
030900040907      * se l'associazione è avvenuta con un record scritto ex-novo xchè
031000030929      *  solo il riferimento era presente.
031100030929     c                   if        ecfTPR = 'GRP'
031200030929     c                   move      'G'           Trovato_Rec
031300030929     c                   End
031400030929      *
031500030829     c                   else
031600040818      * Se NON trovato -------->>
031700040817      *
031800030911      * controlla se c'è il record senza gruppo ancora da associare
031900040818     c                   clear                   Trovato_blk       1
032000040818     c                   Exsr      Prova_con_Blk
032100030829      *
032200040818      * se non ha associato nemmeno al grp=blank  cerca x riferimento
032300040818     c                   if        Trovato_blk <> 'S'
032400030929      *
032500030929      * prova ad associare senza "GRP" se non coincide il GRUPPO ma il riferimento è
032600030929      * presente in archivio TNECF......
032700030929     c                   clear                   Trovato_rif       1
032800040818     c                   Exsr      Prova_con_Rif
032900040817      *
033000040818      **   scarta   **  se nemmeno x riferimento .....
033100040818     c                   if        Trovato_rif <> 'S'
033200040818      *
033300040817      **  a questo punto non è riuscito in nessun caso ad agganciare il record su TNECF
033400040817      **  prova a vedere su TITAS00F se esiste la bolla e non è stata ancora passata in
033500040817      **  Fatturazione. Se così è scrive un record su TNECF con tipologia 'NFT' No Fatturato.
033600040817      **  Se invece non lo trova nemmeno così, allora scrive sempre un record su TNECF con
033700040817      **  tipologia 'NTR' Non Trovato.
033800040818     c                   clear                   found_sped        1
033900040818     c                   exsr      Sped_Bartol
034000040818      *
034100040818      * Se trovata deve scrivere con tipologia 'NFT'
034200040818     c                   iF        found_Sped ='S'
034300040827     c                   exsr      No_ancora_fat
034400040818     c                   eLSe
034500040818     c                   exsr      wrt_Not_Found
034600040818     c                   eNd
034700040817      *
034800030929     c                   End
034900030707     c                   End
035000030707     c                   End
035100030627      *
035200030627     c                   Endsr
035300030707      *----------------------------------------------------------------
035400030707      * Cerca il legame associando i dati
035500030707      *----------------------------------------------------------------
035600030707     c     Associa       Begsr
035700030707      *
035800030829      *  Se il nr.fattura del Partner corrisponde al numero fatt.Partner
035900030829      *  già associato precedentemente deve sommare gli importi
036000040108      *  altrimenti deve scrivere un record nuovo con il nuovo Nr.Fattura con "WRT".
036100030829      *  Letto dai dati del Partner.
036200030829      *
036300091022     c                   If        EcfNfp = *blank  or EcfNfp = EfrNFT OR
036400091022     C                                EcfNTW = 'DPD'
036500030829      *
036600030707      *  imposta la data/nr. fattura Partner e l'importo
036700041104     c                   exsr      EFR_Dati_Ptn
036800041011      **********
036900041011      * Attenzione: se non sono stati aggiornati i campi di fatturazione Bartolini
037000041011      *   deve provare su TITAS a prenderli e se non li trova non deve aggiornare
037100041011      *    EFR e ECF come già elaborati poichè dovrà provare ad elaborarli ancora.
037200041012      **********
037300041012      * E' importante che questo avvenga solo se la data/nr.Fattura Partner conincidano
037400041012      *  fra i 2 files.
037500041011     c                   if        ecfnft = 0 or ecfdft = 0
037600041012     c                   if        ecfnsp > 0 and EcfNfp = EfrNFT and
037700041012     c                             EcfDfp = EfrDFT
037800041011     c                   clear                   par_79r2
037900041011     c                   z-add     ecfaas        paas
038000041011     c                   z-add     ecflnp        plnp
038100041011     c                   z-add     ecfnrs        pnrs
038200041011     c                   z-add     ecfnsp        pnsp
038300080618     c                   move      ecfgrp        pgrp
038400041011     c                   movel     PAR_79r2      kpjbu
038500041011     c                   call      'FIEU79R2'
038600041011     c                   parm                    kpjba
038700041012     c                   movel     kpjbu         par_79r2
038800041011     c                   z-add     pNft          EcfNFt
038900041011     c                   z-add     pDft          EcfDFt
039000041011     c                   z-add     pImr          EcfIMr
039100041011     c                   z-add     pImc          EcfIMc
039200041011     c                   end
039300041011     c                   end
039400041011      **********
039500030707      *  imposta UDATE di elaborazione
039600040908      *    Dicesi INCASELLATA la spedizione che ha sia la fattura Partner che
039700040908      *        la fattura Bartolini
039800040818     C                   Z-ADD     data_Oggi     ecfDTA
039900041011      *
040000041011      * se abbinato ad una fattura Bartolini
040100150325     c                   if        ecfdft > 0
040200030829      * quindi aggiorna il record trovato
040300030724     c                   move      'S'           Trovato_rec
040400151216      *
040500151216      * se lo aveva trovato ed era un ORM
040600151216     c                   if          EcfTPR = 'ORM'
040700151216     c                   move      'O'           Trovato_rec
040800151216     c                   end
040900151216      *
041000041011     c                   else
041100041011      * non lo aggiorna perchè deve rielaborarlo in seguito
041200041011      *  quindi lo deve SCARTARE
041300041011     c                   move      *blank        Trovato_rec
041400041011     c                   end
041500041011      *
041600041103     c                   update    TnEcf4
041700030829      *
041800030829     c                   eLSe
041900030829      * altrimenti:
042000030829      * rilascio il record per poterne scrivere un altro
042100030829     c                   except    aVUOTO
042200030829      *
042300041104      *  imposta la data/nr. fattura Partner e l'importo
042400041104      *      reimposta l'importo del Partner
042500041104     c                   z-add     0             EcfIMP
042600041104     c                   exsr      EFR_Dati_Ptn
042700030829      *
042800040908      *  imposta UDATE di elaborazione INCASELLAMENTO
042900040818     C                   Z-ADD     data_Oggi     ecfDTA
043000030829      *
043100030829     c                   clear                   EcfIMR
043200030829     c                   clear                   EcfIMC
043300040817      *
043400040902      *  prima però controlla che non sia particolare il tipo record trovato
043500040902      *   ossia non sia 'NFT' oppure 'NTR' perchè altrimenti deve creare
043600040902      *   il nuovo record con la stessa tipologia.
043700040902     c                   select
043800040902     c                   when      ecftpr = 'NFT'
043900040902     c                   move      'F'           Trovato_rec
044000040902      *
044100040902     c                   when      ecftpr = 'NTR'
044200040902     c                   move      'N'           Trovato_rec
044300040902      *
044400040902     c                   other
044500040817      *  memorizza che sta scrivendo un nuovo record a parte:
044600040817     c                   move      'W'           Trovato_rec
044700030829     c                   move      'WRT'         EcfTPR
044800040902     c                   endsl
044900030829      *
045000030901      *  salva i dati letti
045100030901     c                   eval      tnecf_SAV = tnecf_read
045200030901      *
045300030901      *   a questo punto cerca il record con il nr. fattura se già stato
045400030901      *  scritto precedentemente:
045500041103     C     Kecf1         Setll     TnEcf04L
045600041103     C     Kecf1         Reade     TnEcf04L
045700030901     c                   move      'N'           Trova_NFP         1
045800030901      *
045900041103     C                   Dow       not %EoF(TnEcf04L)
046000030901      *
046100030901     c                   if        ecfNFP = Sav_ECFNFP and
046200030901     c                             ecfDFP = Sav_ECFDFP
046300030901     c                   move      'S'           Trova_NFP         1
046400030901     C                   z-add     sav_ecfDTA    ecfDTA
046500030901     c                   add       sav_ecfIMP    ecfIMP
046600041103     c                   update    TnEcf4
046700030901     c                   leave
046800030901     c                   end
046900030901      *
047000041103     C     Kecf1         Reade     TnEcf04L
047100030901     c                   endDO
047200030901      *
047300030901      *  Se non l'ha trovato lo deve scrivere:
047400030901     c                   If        Trova_NFP = 'N'
047500030901     c                   eval      tnecf_read = tnecf_SAV
047600041103     c                   write     TnEcf4
047700030901     c                   endIf
047800030901      *
047900030829     c                   eNd
048000030707      *
048100030707     c                   Endsr
048200030902      *----------------------------------------------------------------
048300030902      * Tenta associare il record ad una bolla senza gruppo nè nr.fatt.Partner
048400030902      *----------------------------------------------------------------
048500040818     c     Prova_con_Blk Begsr
048600030902      *
048700041103     C     Kecf_blk      chain     TnEcf04l
048800030902      *
048900030902      * cerca di associare
049000030902      * Se trovato ------------>>
049100041103     c                   If        %Found(TnEcf04L) and
049200030902     c                             EcfNfp = *blank    and
049300030902     c                             EcfDfp = 0
049400030902      *
049500030902      *  imposta la data/nr. fattura Partner e l'importo
049600080618     c                   movel     EfrGRP        EcfGRP
049700041104      *
049800041104      *  imposta la data/nr. fattura Partner e l'importo
049900041104     c                   exsr      EFR_Dati_Ptn
050000030902      *
050100040908      *  imposta UDATE di elaborazione INCASELLAMENTO
050200040818     C                   Z-ADD     data_Oggi     ecfDTA
050300030902      *
050400030902      * si memorizza che tipo record era
050500030902     C                   move      'BLK'         ecfTPR
050600030902      * quindi aggiorna il record trovato
050700030902     c                   move      'S'           Trovato_blk
050800030911     c                   move      'S'           Trovato_rec
050900041103     c                   update    TnEcf4
051000030902      *
051100030902     c                   end
051200030902      *
051300030902     c                   Endsr
051400030929      *----------------------------------------------------------------
051500030929      * Tenta associare il record solo con il riferimento senza il Gruppo
051600030929      *----------------------------------------------------------------
051700040818     c     Prova_con_Rif Begsr
051800030929      *
051900041103     C     Kecf_rif      chain     TnEcf04l
052000030929      *
052100030929      * Se trovato ------------>>
052200041103     c                   If        %Found(TnEcf04L)
052300041104     c                   move      ecftpr        ex_ecftpr
052400030929      * altrimenti:
052500030929      * rilascio il record per poterne scrivere un altro
052600030929     c                   except    aVUOTO
052700030929      *
052800030929      *  imposta la data/nr. fattura Partner e l'importo
052900080618     c                   movel     EfrGRP        EcfGRP
053000041104      *
053100041104      *  imposta la data/nr. fattura Partner e l'importo
053200041104     c                   z-add     0             EcfIMP
053300041104     c                   exsr      EFR_Dati_Ptn
053400030929      *
053500030929      * scrive un nuovo record come "GRP"
053600030929     c                   clear                   EcfIMR
053700030929     c                   clear                   EcfIMC
053800030929     c                   move      'GRP'         EcfTPR
053900041104      *
054000041104      *  imposta UDATE di elaborazione INCASELLAMENTO
054100041104     C                   Z-ADD     data_Oggi     ecfDTA
054200041104      *
054300030929      * quindi aggiorna il record trovato
054400030929     c                   move      'G'           Trovato_rec
054500030929     c                   move      'S'           Trovato_RIF
054600041104      *****
054700041104      *  Attenzione: sorge un problema .....
054800041104      *   Se sto scrivendo un record derivato da un altro NTR o NFT ?!
054900041104     c                   if        ex_ecftpr = 'NTR' or
055000041104     c                             ex_ecftpr = 'NFT'
055100041104     c                   move      ex_ecftpr     EcfTPR
055200041104      * se copia da un NON Trovato
055300041104     c                   if        ex_ecftpr = 'NTR'
055400041104     c                   move      'N'           Trovato_rec
055500041104     C                   Z-ADD     0             ecfDTA
055600041104     c                   end
055700041104      * se copia da un NON Fatturato
055800041104     c                   if        ex_ecftpr = 'NFT'
055900041104     c                   move      'F'           Trovato_rec
056000041104     c                   end
056100041104     c                   end
056200041104      *****
056300041103     c                   write     TnEcf4
056400030929      *
056500030929     c                   end
056600030929      *
056700030929     c                   Endsr
056800030707      *----------------------------------------------------------------
056900040608      *  Cerca di trovare la relativa spedizione Bartolini
057000040608      *----------------------------------------------------------------
057100040608     C     Sped_Bartol   Begsr
057200040608      *
057300040818     c                   clear                   aas_sped
057400040818     c                   clear                   lnp_sped
057500040818     c                   clear                   nrs_sped
057600040818     c                   clear                   nsp_sped
057700040608      * x bolle DPD
057800040608     c                   if        EFRntw = 'DPD'
057900040608      *
058000070927      *  reperisce la spedizione bartolini da TITA437C
058100080618     c                   movel     saveRIF       ta4DPD
058200080618      *
058300060706     c     ta4DPD        chain     tita437C
058400061222     c                   if        %Found(tita437C)
058500040818     c                   eval      aas_Sped = TA4aas
058600040818     c                   eval      lnp_Sped = TA4lnp
058700040818     c                   eval      nrs_Sped = TA4nrs
058800040818     c                   eval      nsp_Sped = TA4nsp
058900040818     c                   eval      found_sped = 'S'
059000040608     c                   else
059100040608      * prova con il RMN direttamente su TITAS
059200040608     c     digits        check     ta4DPD:1
059300040608     c                   if        Not %Found
059400151112     c                   move      ta4DPD        fld14n           14 0
059500151112     c                   z-add     fld14n        fld15n           15 0
059600151112     c     fld15n        chain     titas32C
059700151112     c                   if        %Found(titas32C)
059800040608      * se l'ha trovato imposta i campi della spedizione
059900040818     c                   eval      aas_Sped = TAsaas
060000040818     c                   eval      lnp_Sped = TAslnp
060100040818     c                   eval      nrs_Sped = TAsnrs
060200040818     c                   eval      nsp_Sped = TAsnsp
060300040818     c                   eval      found_sped = 'S'
060400040608     c                   end
060500040608      *
060600040608     c                   end
060700040608     c                   end
060800040608      *
060900040608     c                   endIF
061000040608      *  se Fedex
061100040608     c                   if        EFRntw = 'FED'
061200040608      *
061300070927      *  reperisce la spedizione bartolini da TITA438C
061400070927     c                   movel(P)  efrRIF        ta4RPT
061500070927     c     ta4RPT        chain     tita438C
061600070927     c                   if        %Found(tita438C)
061700040818     c                   eval      aas_Sped = TA4aas
061800040818     c                   eval      lnp_Sped = TA4lnp
061900040818     c                   eval      nrs_Sped = TA4nrs
062000040818     c                   eval      nsp_Sped = TA4nsp
062100040818     c                   eval      found_sped = 'S'
062200040608     c                   end
062300040608      *
062400040608     c                   endIF
062500060303      *
062600130725      * Attenzione x riferimento ci deve essere la nostra bolla quindi
062700130725      *  devono esserci solo dei numeri e deve essere lungo 14 x agganciare
062800060303      *   TITAS.
062900130725      * Altrimenti NON si può considerare la riga.
063000130718     c                   if        EFRntw <>'FED' and
063100130718     c                             EFRntw <>'DPD' and
063200130719     c                             EFRrif <>' '
063300060303      *
063400130725      *   Se c'è il riferimento BRT (efrrif) deve essere di 14 e tutto numerico
063500130725     c                   clear                   fld14            14
063600130725     c                   eval      fld14 = %subst(efrrif:1:14)
063700130725     c     digits        check     fld14
063800130725     c                   if        not %FOUND
063900130719     c                   movel     efrrif        ds_RIF330
064000060303     c     ' ':'0'       xlate     ds_rif330     ds_RIF330
064100130718      *
064200060303      * non deve avere dei blanks nella chiave bolla
064300060303     c                   movel     '20'          k_aas_Sped
064400060303     c                   move      aas_Rif330    k_aas_Sped
064500060303     c                   move      lnp_Rif330    k_lnp_Sped
064600060303     c                   move      nrs_Rif330    k_nrs_Sped
064700060303     c                   move      nsp_Rif330    k_nsp_Sped
064800060303     c     Ktas_Bolla    chain     titas30c
064900060303     c                   if        %found(titas30C)
065000060303     c                   eval      aas_Sped = TASaas
065100060303     c                   eval      lnp_Sped = TASlnp
065200060303     c                   eval      nrs_Sped = TASnrs
065300060303     c                   eval      nsp_Sped = TASnsp
065400060303     c                   eval      found_sped = 'S'
065500060303     c                   end
065600130725     c                   endif
065700060303      *
065800060303     c                   endIF
065900060303      *
066000040818      *  Se è riuscito ad agganciare la spedizione,
066100040818      *   deve controllare che al limite sia sul TITAS00F e quindi non sia stata ancora
066200040818      *    fatturata..... altrimenti non l'ha trovata.
066300040818     c                   if        found_sped = 'S'
066400040818      *
066500040818      * A questo punto controlla su Titas00F se presente
066600040818     c     Ktas          chain     titas30C
066700040818      * deve essere solo sul record di TITAS00F per non essere ancora fatturata
066800040818     c                   if        %found(titas30C) and *in12 = *on or
066900040818     c                             %found(titas30C) and *in13 = *on or
067000040818     c                             not %found(titas30C)
067100040907      *
067200040907      * non essendo ancora fattura il nr./data fattura Bartolini
067300040907      *   non devono esserci sul record.
067400040818     c                   clear                   found_sped
067500040818     C                   Endif
067600040818      *
067700040818     C                   Endif
067800040818      *
067900040818     c                   Endsr
068000040818      *----------------------------------------------------------------
068100040827      *  Scrive x Non ancora fatturata
068200040818      *----------------------------------------------------------------
068300040827     C     No_ancora_fat Begsr
068400040818      *
068500040827     c                   move      'F'           Trovato_rec
068600040827      *
068700041011     c                   clear                   PAR_79r1
068800040827     c                   move      Aas_Sped      kAas
068900040827     c                   move      Lnp_Sped      kLnp
069000040827     c                   move      Nrs_Sped      kNrs
069100040827     c                   move      Nsp_Sped      kNsp
069200040827     c                   move      'NFT'         KTpr
069300040827     c                   move      efrNft        kNfp
069400041104      ***
069500041104      ***  x DPD non servono i dettagli della fattura quindi impostiamo a (1)
069600041104      ***   sempre il nr.fattura del Partner
069700041104     c                   if        efrntw ='DPD'
069800041104     c                   move(p)   1             KNfp
069900041104     c                   end
070000041104      ***
070100040827     c                   move      efrDft        kDfp
070200040827     c                   move      efrGrp        kGrp
070300040827     c                   move      efrImp        kImp
070400040827      *
070500040827      * Il pgm scrive il record anomalo con tipologia "NFT"
070600041011     c                   movel     PAR_79r1      kpjbu
070700040827     c                   call      'FIEU79R1'
070800040827     c                   parm                    kpjba
070900040827      *
071000040827     c                   Endsr
071100040827      *----------------------------------------------------------------
071200040827      *  Scrive x Non Trovato
071300040827      *----------------------------------------------------------------
071400040827     C     Wrt_Not_Found Begsr
071500040827      *
071600040818      *  Scrive comunque un record particolare con solo i dati del Partner su TNECF
071700041103     c                   clear                   TnEcf4
071800040818      *
071900040818      * Se non trovata deve scrivere con tipologia 'NTR'
072000040818     c                   move      'N'           Trovato_rec
072100040818     c                   move      'NTR'         EcfTPR
072200151216      *---------
072300151216      *  imposta la data/nr. fattura Partner e l'importo
072400151216     c                   movel     EfrNTW        EcfNTW
072500151216     c                   movel     EfrRIF        EcfRIF
072600151216     c                   movel     EfrGRP        EcfGRP
072700151216      *
072800151216      *  imposta la data/nr. fattura Partner e l'importo
072900151216      *      reimposta l'importo del Partner
073000151216     c                   z-add     0             EcfIMP
073100151216     c                   exsr      EFR_Dati_Ptn
073200151216      *
073300151216      *---------
073400040818      *
073500150323      * ? ATTENZIONE  x DPD, se il parcel è uno 0844,
073600150323      * ?  POTREBBERO ESSERE ORM ITALIA-ITALIA
073700150323     c                   if        efrNTW = 'DPD'
073800150323     c                              and
073900150323     c                             %subst(efrRIF:1:4)='0844'
074000150323     c                   movel     efrRIF        Key14            14
074100150323     c     Key14         chain     fidpo06l
074200150323     c                   if        %Found(Fidpo06l)
074300150323     c                   move      'O'           Trovato_rec
074400150323     c                   move      'ORM'         EcfTPR
074500150323     c                   move      RIF_fidpORM   EcfRMN
074600151215      *
074700151215     c     RIF_fidpORM   setll     tita432c
074800151215     c     RIF_fidpORM   reade     tita432c
074900151215     c                   dow       not %Eof(tita432C)
075000151215      *
075100151215     c                   if        ta4trc = 'M'
075200151215     c                   z-add     ta4aas        ecfaas
075300151215     c                   z-add     ta4lnp        ecflnp
075400151215     c                   z-add     ta4nrs        ecfnrs
075500151215     c                   z-add     ta4nsp        ecfnsp
075600151215     C     KTasECF       chain     titas30c
075700151215     c                   if        %Found(titas30c)
075800151217     c                   if         tasTBL = 'A3' or
075900151217     c                              tasTBL = 'F3' or
076000151217     c                              tasCCA = '7'
076100151217      *** non deve associare alla nostra bolla
076200151217     c                   clear                   ecfaas
076300151217     c                   clear                   ecflnp
076400151217     c                   clear                   ecfnrs
076500151217     c                   clear                   ecfnsp
076600151217     c                   else
076700151216     C                   EXSR      campi_ECF
076800151217     c                   end
076900151215     c                   end
077000151215      *
077100151215     c                   leave
077200151215     c                   end
077300151215      *
077400151215     c     RIF_fidpORM   reade     tita432c
077500151215     c                   end
077600150323     c                   end
077700150323     c                   end
077800151216      *
077900040907     C*** Non deve essere incasellata: 7/9/04
078000041104     C                   Z-ADD     0             EcfDTA
078100041103     c                   write     TnEcf4
078200040818      *
078300040818     c                   Endsr
078400151216      *----------------------------------------------------------------
078500151216     c     EFR_Dati_Ptn  Begsr
078600151216      *
078700160628      *  Causale sui primi 3 del campo Filler
078800160628     c                   if        %subst(ecfflr:1:3)= *blank
078900160628     c                   eval       %subst(ecfflr:1:3)= efrcau
079000160628     c                   else
079100160628      * Forza se ORM o RESO o ISOLA
079200160628     c                   if        %subst(efrcau:1:1)= 'O' or
079300160628     c                             %subst(efrcau:1:1)= 'R' or
079400160628     c                             %subst(efrcau:3:1)= 'I'
079500160628     c                   eval       %subst(ecfflr:1:3)= efrcau
079600160628     c                   end
079700160628     c                   end
079800160628      *
079900041104      *  imposta la data/nr. fattura Partner e l'importo
080000041104     c                   move      EfrNFT        EcfNFp
080100041104      ***
080200041104      ***  x DPD non servono i dettagli della fattura quindi impostiamo a (1)
080300041104      ***   sempre il nr.fattura del Partner
080400041104     c                   if        efrntw ='DPD'
080500041104     c                   move(p)   1             EcfNFp
080600041104     c                   end
080700041104      ***
080800041104     c                   z-add     EfrDFT        EcfDFp
080900041104      **
081000041104      ** Attenzione qui si aggiunge sempre
081100041104      **   quindi se si deve reimpostare il valore prima del richiamo
081200041104      **    alla routine va pulito il campo.
081300041104     c                   add       EfrIMP        EcfIMP
081400041104      ***---
081500041104     c                   Endsr
081600151216      * =======================================================================
081700151216      **  SE ORM DPD commissionato in ITalia da Consegnare in Italia (no linee DPD)
081800151216      * =======================================================================
081900151216     c     campi_ECF     Begsr
082000151216      *
082100151216     c                   movel     tasLNA        ECFLNA
082200151216     c                   movel     tasMGS        ECFMGS
082300151216     c                   movel     tasTBL        ECFTBL
082400151216     c                   movel     tasFBR        ECFFBR
082500151216     c                   movel     tasKSC        ECFKSC
082600151216     c                   movel     tasDTI        ECFDTI
082700151216     c                   movel     tasDCM        ECFDCM
082800151216     c                   movel     tasCCA        ECFCCA
082900151216     c                   movel     tasNCL        ECFNCL
083000151216     c                   movel     tasPKF        ECFPKF
083100151216     c                   movel     tasVLF        ECFVLF
083200151216     c                   movel     tasNFT        ECFNFT
083300151216     c                   movel     tasDFT        ECFDFT
083400151216      *
083500151216     c                   clear                   Grp
083600151216     c                   clear                   ImR
083700151216     c                   clear                   ImC
083800151216      *
083900151216      * Imposta i Ricavi
084000151216     c                   if        tastbl <> 'AS' and tastbl <> 'FS' and
084100151216     c                             tastbl <> 'AP'
084200151216     c                   exsr      Ricavi
084300151216     c                   end
084400151216      *
084500151216      * Imposta le Competenze
084600151216     c                   exsr      Competenze
084700151216      *
084800151216      * Ricavi/Competenze
084900151216     c                   z-add(h)  ImR           ECFIMR
085000151216     c                   z-add(h)  ImC           ECFIMC
085100151216      *
085200151216     c                   Endsr
085300151216      * =======================================================================
085400151216      *  imposta i Ricavi raggruppati su schiere
085500151216      * =======================================================================
085600151216     c     Ricavi        Begsr
085700151216      *
085800151216      * IMPORTO Ricavo
085900151216      *  Porto
086000151216     c                   if        tasPOR > 0
086100151216     c                   clear                   evaVAR
086200151216     c                   movel     '1'           evaVAR
086300151216     c                   z-add     tasPOR        importo
086400151216     c                   exsr      Tbe_EVA
086500151216     c                   end
086600151216      *  Varie 1
086700151216     c                   if        tasVA1 > 0
086800151216     c                   clear                   evaVAR
086900151216     c                   movel     tasSV1        evaVAR
087000151216     c                   z-add     tasVA1        importo
087100151216     c                   exsr      Tbe_EVA
087200151216     c                   end
087300151216      *  Varie 2
087400151216     c                   if        tasVA2 > 0
087500151216     c                   clear                   evaVAR
087600151216     c                   movel     tasSV2        evaVAR
087700151216     c                   z-add     tasVA2        importo
087800151216     c                   exsr      Tbe_EVA
087900151216     c                   end
088000151216      *  Varie 3
088100151216     c                   if        tasVA3 > 0
088200151216     c                   clear                   evaVAR
088300151216     c                   movel     tasSV3        evaVAR
088400151216     c                   z-add     tasVA3        importo
088500151216     c                   exsr      Tbe_EVA
088600151216     c                   end
088700151216      *  Altre Varie
088800151216      *
088900151216      * Controlla quindi sul file VARIE se ci sono altre varie da aggiungere
089000151216      *  e raggruppare.
089100151216     c     ktas3         setll     tita730c
089200151216     c     ktas3         reade     tita730c
089300151216     c                   dow       not %Eof(tita730c)
089400151216     c                   clear                   evaVAR
089500151216     c                   movel     ta7SVn        evaVAR
089600151216     c                   z-add     ta7VAn        importo
089700151216     c                   exsr      Tbe_EVA
089800151216     c     ktas3         reade     tita730c
089900151216     c                   enddo
090000151216      *
090100151216     c                   Endsr
090200151216      *----------------------------------------------------------------
090300151216      *  Decodifica il raggruppamento per le Varie
090400151216      *----------------------------------------------------------------
090500151216     c     TBE_EVA       Begsr
090600151216      *
090700151216     c                   clear                   evaNTW
090800151216     c                   clear                   evaFIL
090900151216     c                   move      EcfGRP        Gruppo            3
091000151216      *
091100151216      * prova se ci sono delle particolarità sul Partner + LNP/A
091200151216      *  chiave completa  x LNP
091300151216     c                   movel     ECFNtw        evaNTW
091400151216     c                   move      ecfLNP        evaFIL
091500151216     c     kEVA          setll     tnEVA01l
091600151216      *
091700151216      *  chiave completa   x LNA
091800151216     C                   IF        not %EQUAL
091900151216     c                   movel     ECFNtw        evaNTW
092000151216     c                   move      ecfLNA        evaFIL
092100151216     c     kEVA          setll     tnEVA01l
092200151216      *
092300151216     C                   IF        not %EQUAL
092400151216      * se ci sono dei codici particolari da prendere in considerazione
092500151216      *  al posto di quelli standard.
092600151216      *  chiave parziale
092700151216     c                   movel     ECFNtw        evaNTW
092800151216     c                   clear                   evaFIL
092900151216     c     kEVA          setll     tnEVA01l
093000151216     C                   IF        not %EQUAL
093100151216     c                   clear                   evaNTW
093200151216     c                   clear                   evaFIL
093300151216     c     kEVA          setll     tnEVA01l
093400151216     c                   end
093500151216     c                   endIF
093600151216     c                   endIF
093700151216      *
093800151216     C                   IF        %EQUAL
093900151216      *
094000151216     C                   DOU       %EOF
094100151216     C     kEVA          Reade     tnEVA01l
094200151216     C                   IF        NOT %EOF and
094300151216     C                             evaDDA <= data_oggi and
094400151216     C                             evaADA >= data_oggi
094500151216     c                              and
094600151216     c                             evaGRP  = Gruppo
094700151216     c                   leave
094800151216     C                   ENDIF
094900151216     C                   ENDDO
095000151216      *
095100151216      * Totalizza l'importo del ricavo x il gruppo di appartenenza
095200151216      *  legato alla tabella EVA.
095300151216     c                   add       Importo       ImR
095400151216      *
095500151216     c                   end
095600151216      *
095700151216     c                   Endsr
095800151216      *----------------------------------------------------------------
095900151216      *  imposta le Competenze raggruppate su schiere
096000151216      *----------------------------------------------------------------
096100151216     c     Competenze    Begsr
096200151216      *
096300151216     c                   clear                   ImpV0
096400151216     c                   clear                   ImpVP
096500151216     c                   clear                   CodV0
096600151216     c                   clear                   CodVP
096700151216      *
096800151216      * IMPORTO Competenze
096900151216     c     ktas3         chain     Eccev30C
097000151216     c                   if        %Found(Eccev30C)
097100151216      *
097200151216     C                   Do        50            CV                3 0
097300151216      *
097400151216     C                   If        *in01 = *on
097500151216      * esce quando non ha più Voci in skiera
097600151216     C                   If        CodV0(CV) = 0
097700151216     c                   leave
097800151216     c                   end
097900151216     c                   clear                   evoVOC
098000151216     c                   movel     CodV0(CV)     evoVOC
098100151216     c                   z-add     ImpV0(CV)     importo
098200151216     c                   exsr      Tbe_EVO
098300151216     c                   end
098400151216      *
098500151216     C                   If        *in02 = *on
098600151216      * esce quando non ha più Voci in skiera
098700151216     C                   If        CodVP(CV) = 0
098800151216     c                   leave
098900151216     c                   end
099000151216     c                   clear                   evoVOC
099100151216     c                   movel     CodVP(CV)     evoVOC
099200151216     c                   z-add     ImpVP(CV)     importo
099300151216     c                   exsr      Tbe_EVO
099400151216     c                   end
099500151216      *
099600151216     c                   endDo
099700151216      *
099800151216     C                   endIF
099900151216      *
100000151216     c                   Endsr
100100151216      *----------------------------------------------------------------
100200151216      *  Decodifica il raggruppamento per le Voci
100300151216      *----------------------------------------------------------------
100400151216     c     TBE_EVO       Begsr
100500151216      *
100600151216     c                   clear                   evoNTW
100700151216     c                   clear                   evoFIL
100800151216     c                   move      ecfgrp        Gruppo            3
100900151216      *
101000151216      * prova se ci sono delle particolarità sul Partner + LNP/A
101100151216      *  chiave completa
101200151216     c                   movel     ecfntw        evoNTW
101300151216     c                   move      ecfLNP        evoFIL
101400151216     c     kEVO          setll     tnEVO01l
101500151216      *  chiave completa
101600151216     C                   IF        not %EQUAL
101700151216     c                   movel     ecfntw        evoNTW
101800151216     c                   move      ecfLNA        evoFIL
101900151216     c     kEVO          setll     tnEVO01l
102000151216     C                   IF        not %EQUAL
102100151216      * se ci sono dei codici particolari da prendere in considerazione
102200151216      *  al posto di quelli standard.
102300151216      *  chiave parziale
102400151216     c                   movel     ecfNtw        evoNTW
102500151216     c                   clear                   evoFIL
102600151216     c     kEVO          setll     tnEVO01l
102700151216     C                   IF        not %EQUAL
102800151216     c                   clear                   evoNTW
102900151216     c                   clear                   evoFIL
103000151216     c     kEVO          setll     tnEVO01l
103100151216     c                   end
103200151216     c                   endIF
103300151216     c                   endIF
103400151216      *
103500151216     C                   IF        %EQUAL
103600151216      *
103700151216     C                   DOU       %EOF
103800151216     C     kEVO          Reade     tnEVO01l
103900151216     C                   IF        NOT %EOF        and
104000151216     C                             evoDDA <= data_oggi and
104100151216     C                             evoADA >= data_oggi
104200151216     c                              and
104300151216     c                             evoGRP  = Gruppo
104400151216     c                   leave
104500151216     C                   ENDIF
104600151216     C                   ENDDO
104700151216      *
104800151216      * Incasella l'importo del ricavo nel gruppo di appartenenza
104900151216      *  legato alla tabella EVO.
105000151216     c                   add       Importo       ImC
105100151216     c                   endIF
105200151216      *
105300151216     c                   Endsr
105400041104      *----------------------------------------------------------------
105500160203      *  Controlla gli ORM Italia Italia da parte BRT e NON DPD
105600160203      *----------------------------------------------------------------
105700160203     c     ORM_ITA_BRT   Begsr
105800160203      *
105900160203      ** -----------
106000160203     C/EXEC SQL
106100160203     C+ Declare Tas_BOLLA Cursor for
106200160205     c+  select * from (
106300160204     c+  SELECT * FROM (SELECT * FROM titas00f
106400160204     c+    JOIN   tita400f
106500160204     c+      ON   digits(tasaas) concat digits(taslnp) concat
106600160204     c+           digits(tasnrs) concat digits(tasnsp)
106700160204     c+       =   digits(ta4aas) concat digits(ta4lnp) concat
106800160204     c+           digits(ta4nrs) concat digits(ta4nsp)
106900160204     c+  WHERE TASDCM BETWEEN :d1daAMG AND :d1AMG
107000160204     c+        and tascca <> '7'  AND tastbl NOT IN ('A3', 'F3')
107100160204     c+        and TA4TRC ='A' and ta4not like 'DPD%' and
107200160204     c+  digits(tasaas) concat digits(taslnp) concat
107300160204     c+  digits(tasnrs) concat digits(tasnsp)        NOT IN ( SELECT
107400160204     c+  digits(ecfaas) concat digits(ecflnp) concat
107500160204     c+  digits(ecfnrs) concat digits(ecfnsp)        FROM tnecf00f
107600160204     c+  WHERE ecfntw='DPD') )  bolle
107700160204     c+    JOIN   fidpo00f
107800160204     c+      ON   SUBSTR(ta4not, 4, 10)
107900160204     c+       =   dpodep concat digits(dpoord)
108000160204     c+  WHERE dposts='12'and dponrp  NOT IN
108100160205     c+    (SELECT ecfrif from tnecf00f  WHERE ecfntw='DPD') ) B
108200160203     C/END-EXEC
108300160203      *
108400160203     C/EXEC SQL
108500160203     C+ OPEN Tas_BOLLA
108600160203     C/END-EXEC
108700160203      *
108800160203     C                   DOU       SqlCod <> 0
108900160203     C/EXEC SQL
109000160205     C+ FETCH NEXT FROM Tas_BOLLA INTO
109100160205     c+                  :dsTAS, :dsTA4, :dsDPO
109200160203     C/END-EXEC
109300160203     C                   SELECT
109400160203     c*
109500160203     c* a fine file Totali x rotture
109600160203     C                   WHEN      SqlCod = 100
109700160203     c                   if        almeno_uno ='S'
109800160203     c                   else
109900160203     c                   exsr      NO_dati
110000160203     c                   end
110100160203     c                   leave
110200160203     **
110300160203     C                   WHEN      SqlCod < 0
110400160203     C                   seton                                        H1
110500160203     c                   RETURN
110600160203     c*
110700160203     C                   OTHER
110800160203     c* DETTAGLIO
110900160203     c                   move      'S'           almeno_uno        1
111000160203      *
111100160203     c                   clear                   kpjbu
111200160203     c                   movel     d1Ntw         KNtw
111300160205     c                   movel     sql_tasAAS    tasAASprm
111400160205     c                   movel     sql_tasLnp    tasLnpprm
111500160205     c                   movel     sql_tasNrs    tasNrsprm
111600160205     c                   movel     sql_tasNsp    tasNspprm
111700160205     c                   movel     sql_tasTbl    tasTblprm
111800160203     c                   movel     parBolla      kpjbu
111900160203      *
112000160203     c                   call      'FIEU78R5'
112100160203     c                   parm                    kpjba
112200160203     c                   parm                    DsTAS
112300160204     c                   parm                    DsDPO
112400160203      *
112500160203     C                   ENDSL
112600160203      *
112700160203     C                   ENDDO
112800160203     C/EXEC SQL
112900160203     C+ CLOSE Tas_BOLLA
113000160203     C/END-EXEC
113100160204      *
113200160204      ** -----------
113300160204     C/EXEC SQL
113400160204     C+ Declare Tas1_BOLLA Cursor for
113500160205     c+  SELECT * FROM (
113600160204     c+  SELECT * FROM (SELECT * FROM titas10f
113700160204     c+    JOIN   tita400f
113800160204     c+      ON   digits(tasaas) concat digits(taslnp) concat
113900160204     c+           digits(tasnrs) concat digits(tasnsp)
114000160204     c+       =   digits(ta4aas) concat digits(ta4lnp) concat
114100160204     c+           digits(ta4nrs) concat digits(ta4nsp)
114200160204     c+  WHERE TASDCM BETWEEN :d1daAMG AND :d1AMG
114300160204     c+        and tascca <> '7'  AND tastbl NOT IN ('A3', 'F3')
114400160204     c+        and TA4TRC ='A' and ta4not like 'DPD%' and
114500160204     c+  digits(tasaas) concat digits(taslnp) concat
114600160204     c+  digits(tasnrs) concat digits(tasnsp)        NOT IN ( SELECT
114700160204     c+  digits(ecfaas) concat digits(ecflnp) concat
114800160204     c+  digits(ecfnrs) concat digits(ecfnsp)        FROM tnecf00f
114900160204     c+  WHERE ecfntw='DPD') )  bolle
115000160204     c+    JOIN   fidpo00f
115100160204     c+      ON   SUBSTR(ta4not, 4, 10)
115200160204     c+       =   dpodep concat digits(dpoord)
115300160204     c+  WHERE dposts='12'and dponrp  NOT IN
115400160205     c+    (SELECT ecfrif from tnecf00f  WHERE ecfntw='DPD') ) B
115500160204     C/END-EXEC
115600160204      *
115700160204     C/EXEC SQL
115800160204     C+ OPEN Tas1_BOLLA
115900160204     C/END-EXEC
116000160204      *
116100160204     C                   DOU       SqlCod <> 0
116200160204     C/EXEC SQL
116300160205     C+ FETCH NEXT FROM Tas1_BOLLA INTO
116400160205     c+                  :dsTAS, :dsTA4, :dsDPO
116500160204     C/END-EXEC
116600160204     C                   SELECT
116700160204     c*
116800160204     c* a fine file Totali x rotture
116900160204     C                   WHEN      SqlCod = 100
117000160204     c                   if        almeno_uno ='S'
117100160204     c                   else
117200160204     c                   exsr      NO_dati
117300160204     c                   end
117400160204     c                   leave
117500160204     **
117600160204     C                   WHEN      SqlCod < 0
117700160204     C                   seton                                        H1
117800160204     c                   RETURN
117900160204     c*
118000160204     C                   OTHER
118100160204     c* DETTAGLIO
118200160204     c                   move      'S'           almeno_uno        1
118300160204      *
118400160204     c                   clear                   kpjbu
118500160204     c                   movel     d1Ntw         KNtw
118600160205     c                   movel     sql_tasAAS    tasAASprm
118700160205     c                   movel     sql_tasLnp    tasLnpprm
118800160205     c                   movel     sql_tasNrs    tasNrsprm
118900160205     c                   movel     sql_tasNsp    tasNspprm
119000160205     c                   movel     sql_tasTbl    tasTblprm
119100160204     c                   movel     parBolla      kpjbu
119200160204      *
119300160204     c                   call      'FIEU78R5'
119400160204     c                   parm                    kpjba
119500160204     c                   parm                    DsTAS
119600160204     c                   parm                    DsDPO
119700160204      *
119800160204     C                   ENDSL
119900160204      *
120000160204     C                   ENDDO
120100160204     C/EXEC SQL
120200160204     C+ CLOSE Tas1_BOLLA
120300160204     C/END-EXEC
120400160203     c                   Endsr
120500160203      *----------------------------------------------------------------
120600160203      *
120700160203      *----------------------------------------------------------------
120800160203     c     NO_DATI       begSR
120900160203      *
121000160203     c                   endSR
121100160203      *----------------------------------------------------------------
121200041103     oTNECF4    e            aVUOTO
