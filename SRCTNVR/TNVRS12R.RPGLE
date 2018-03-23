001100021216     FSISDOB    UF A E           K DISK
001200021216     F                                     RENAME(SISDO000:SISDOBDG)
001300001205     f                                     PREFIX(b_)
001700001205     F*---
001800021219     F* Saldi cliente in UNITRAGRU
001900001205     F*---
002000021219     FSISDOU    IF   E           K DISK
002100021219     F                                     RENAME(SISDO000:SISDOUNI)
002200021216     f                                     PREFIX(u_)
002600001205     D*------------------------------------------------------------------------*
002700001205     D* DATA STRUCTURE
002800001205     D*------------------------------------------------------------------------*
002900001205     D KDOKSC          S                   LIKE(B_SDOKSC)
003000001205     D KDOANN          S                   LIKE(B_SDOANN)
003100001205     D KDOMES          S                   LIKE(B_SDOMES)
004300021216     D*-------------------
004400021216     D* ARCHITETTURA
004500021216     D*-------------------
004600021216     D KPJBA         E DS
004700021216     D*-------------------
004800021216     D* PARAMETRI IN ENTRATA
004900021216     D*-------------------
005000021216     D PARAM           DS
005100021216     D  PARORD                 1      1
005200021216     D  PARFIL                 2      4  0
005300021216     D  PARKS1                 5     11  0
005400021216     D  PARDS1                12     19  0
005500021216     D  PARAA1                12     15  0
005600021216     D  PARMG1                16     19  0
005700021218     D  PARME1                16     17  0
005800140530     D  PARgg1                18     19  0
005900021216     D  PARDS2                20     27  0
006000021216     D  PARAA2                20     23  0
006100021216     D  PARMG2                24     27  0
006200021219     D  PARME2                24     25  0
006300140530     D  PARgg2                26     27  0
006400021216     D  PARDF1                28     35  0
006500021216     D  PARDF2                36     43  0
006600021216     D  PARKS2                44     50  0
006700021216     D  PARASI                51     54  0
006800140530     D  PARlib                55     64
006900001205     C*------------------------------------------------------------------------*
007000001205     C* MAIN LINE
007100140529     d Wdata8          DS
007200140529     d  dadata                 1      8  0
007300140529     d  adata                  9     16  0
007400140529     d  GioLav                17     21  0
008400140529     d ggme1           s              2  0
008500140529     d ggme2           s              2  0
008600140529     d ggperme1        s              2  0
008700140529     d ggperme2        s              2  0
008800140529     d dataiso         s               d
008900001205     C*------------------------------------------------------------------------*
009000001205     C*----
009100001205     C* SALDI CLIENTE TESTATA
009200001205     C*----
009300021219     C                   READ      SISDOU                                 99
009400001205DO  1C     *IN99         DOWEQ     *OFF
009500140530     c                   movel     u_sdoann      c_data            6 0
009600140530     c                   move      u_sdomes      c_data
009700140530     c                   exsr      srdata
009800140530IF  2C                   IF        dataui >= pards1 and
009900140529     C                             datauf <= pards2
010201160902     C                   if        U_SDOcln <> ' '
010300021216     C                   Z-ADD     U_SDOKSC      KDOKSC
010400130228     C                   EVAL      KDOANN = parasi
010500021216     C                   Z-ADD     U_SDOMES      KDOMES
010600021216     C     KEYSDO        CHAIN     SISDOB                             98
012300021219IF  4C     *IN98         IFEQ      *OFF
012601160902     C                   movel     U_SDOcln      b_SDOcln
012700001207     C                   UPDATE    SISDOBDG
013900021219E   2C                   ENDIF
013901160902E   2C                   ENDIF
013902160902E   2C                   ENDIF
014000021219     C                   READ      SISDOU                                 99
014100001205E   1C                   ENDDO
021000001205     C*
021100001205     C                   SETON                                            LR
021200001205     C*------------------------------------------------------------------------*
021300140530     C* srdata  imposta data periodo del file saldi
021400001205     C*------------------------------------------------------------------------*
021500140530     C     srdata        BEGSR
021600140530     c* per prendere l'intero mese dei saldi devo impostare la data inizio
021700140530     c* con il gg di FINE mese, mentre la data fine con il gg di INIZIO mese
021800140530     c                   movel     c_data        dataui            8 0
021900140530     c                   movel     c_data        datauf            8 0
022000140530     c                   move      01            datauf
022100140530     c                   movel     datauf        dataiso
022200140530     c                   adddur    1:*m          dataiso
022300140530     c                   subdur    1:*d          dataiso
022400140530     c                   move      dataiso       dataui
022500140530     c                   endsr
024600140528     C*------------------------------------------------------------------------*
024700140528     C* *INZSR - OPERAZIONI INIZIALI
024800140528     C*------------------------------------------------------------------------*
024900140528     C     *INZSR        BEGSR
025000021216     C*
025100021216     C* RICEVIMENTO PARAMETRI
025200021216     C     *ENTRY        PLIST
025300021216     C                   PARM                    KPJBA
025400021216     C                   MOVEL     KPJBU         PARAM
025500140528     c* verifico i gg lavorativi per poter riproporzionare le rettifiche
025600140528     c* dell'intero mese nei gg richiesti per la simulazione
025700140530     c*
025800140530     c                   if        pargg1 = 0
025900140530     c                   eval      pargg1 = 01
026000140530     c                   end
026100140530     c                   if        pargg2 = 0
026200140530     c                   eval      pargg2 = 01
026300140530     c                   move      pards2        dadata
026400140530     c                   movel     dadata        dataiso
026500140530     c                   adddur    1:*m          dataiso
026600140530     c                   subdur    1:*d          dataiso
026700140530     c                   move      dataiso       pards2
026800140530     c                   end
026900140528     C*
027000140528      *calcolo i gg lavorativi del mese di inizio
027100140530     c                   eval      dadata = pards1
027200140530     c                   move      01            dadata
027300140528     c                   movel     dadata        dataiso
027400140528     c                   adddur    1:*m          dataiso
027500140528     c                   subdur    1:*d          dataiso
027600140528     c                   move      dataiso       adata
027700140528     c                   CALL      'XSRLAV8'
027800140528     c                   PARM                    Wdata8
027900140528     c                   eval      ggme1 = giolav
028000140528      *calcolo i gg lavorativi del periodo
028100140528     c                   select
028200140528     c                   when      paraa1 = paraa2 and parme1 = parme2
028300140528     c                   eval      dadata = pards1
028400140528     c                   eval      adata = pards2
028500140528     c                   CALL      'XSRLAV8'
028600140528     c                   PARM                    Wdata8
028700140529     c                   eval      ggperme1 = giolav
028800140528     c                   when      parme1 <> parme2
028900140528      *calcolo i gg lavorativi del periodo per il mese di inizio
029000140528     c                   eval      dadata = pards1
029100140528     c                   movel     dadata        dataiso
029200140528     c                   adddur    1:*m          dataiso
029300140528     c                   subdur    1:*d          dataiso
029400140528     c                   move      dataiso       adata
029500140528     c                   CALL      'XSRLAV8'
029600140528     c                   PARM                    Wdata8
029700140528     c                   eval      ggperme1 = giolav
029800140528      *calcolo i gg lavorativi del mese di fine
029900140530     c                   eval      dadata = pards2
030000140530     c                   move      01            dadata
030100140528     c                   movel     dadata        dataiso
030200140528     c                   adddur    1:*m          dataiso
030300140528     c                   subdur    1:*d          dataiso
030400140528     c                   move      dataiso       adata
030500140528     c                   CALL      'XSRLAV8'
030600140528     c                   PARM                    Wdata8
030700140528     c                   eval      ggme2 = giolav
030800140528      *calcolo i gg lavorativi del periodo per il mese di fine
030900140530     c                   eval      dadata = pards2
031000140530     c                   move      01            dadata
031100140528     c                   eval      adata = pards2
031200140528     c                   CALL      'XSRLAV8'
031300140528     c                   PARM                    Wdata8
031400140528     c                   eval      ggperme2 = giolav
031500140528     c                   endsl
031600001205     C*
031700001205     C* CHIAVI DI LETTURA
031800001205     C     KEYSDO        KLIST
031900001205     C                   KFLD                    KDOKSC
032000001205     C                   KFLD                    KDOANN
032100001205     C                   KFLD                    KDOMES
033400001205     C*
033500001205     C                   ENDSR
